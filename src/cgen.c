//
//  Copyright (C) 2011-2013  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "util.h"
#include "phase.h"
#include "lib.h"
#include "common.h"
#include "rt/signal.h"
#include "rt/rt.h"

#include <stdlib.h>
#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>

#undef NDEBUG
#include <assert.h>

static LLVMModuleRef  module = NULL;
static LLVMBuilderRef builder = NULL;
static LLVMValueRef   mod_name = NULL;

static ident_t var_offset_i = NULL;
static ident_t local_var_i = NULL;
static ident_t global_const_i = NULL;
static ident_t sig_struct_i = NULL;
static ident_t foreign_i = NULL;
static ident_t never_waits_i = NULL;

// Linked list of entry points to a process
// These correspond to wait statements
struct proc_entry {
   int               state_num;
   tree_t            wait;
   LLVMBasicBlockRef bb;
   struct proc_entry *next;
};

// Linked list of named blocks such as loops
struct block_list {
   ident_t            name;
   LLVMBasicBlockRef  exit_bb;
   LLVMBasicBlockRef  entry_bb;
   struct block_list *next;
};

// Code generation context for a process or function
typedef struct cgen_ctx {
   struct proc_entry *entry_list;
   struct block_list *blocks;
   LLVMValueRef      state;
   LLVMValueRef      fn;
   tree_t            proc;
   tree_t            fdecl;
} cgen_ctx_t;

static LLVMValueRef cgen_expr(tree_t t, cgen_ctx_t *ctx);
static void cgen_stmt(tree_t t, cgen_ctx_t *ctx);
static LLVMValueRef cgen_get_var(tree_t decl, cgen_ctx_t *ctx);
static bool cgen_const_bounds(type_t type);
static LLVMValueRef cgen_array_data_ptr(type_t type, LLVMValueRef var);
static LLVMTypeRef cgen_signal_type(type_t type);
static LLVMValueRef cgen_var_lvalue(tree_t t, cgen_ctx_t *ctx);
static LLVMValueRef cgen_const_record(tree_t t, cgen_ctx_t *ctx);
static int cgen_array_dims(type_t type);

static LLVMValueRef llvm_int1(bool b)
{
   return LLVMConstInt(LLVMInt1Type(), b, false);
}

static LLVMValueRef llvm_int8(int8_t i)
{
   return LLVMConstInt(LLVMInt8Type(), i, false);
}

static LLVMValueRef llvm_int32(int32_t i)
{
   return LLVMConstInt(LLVMInt32Type(), i, false);
}

static LLVMValueRef llvm_int64(int64_t i)
{
   return LLVMConstInt(LLVMInt64Type(), i, false);
}

static LLVMValueRef llvm_real(double r)
{
   return LLVMConstReal(LLVMDoubleType(), r);
}

static LLVMTypeRef llvm_void_ptr(void)
{
   return LLVMPointerType(LLVMInt8Type(), 0);
}

static LLVMValueRef llvm_void_cast(LLVMValueRef ptr)
{
   return LLVMBuildPointerCast(builder, ptr, llvm_void_ptr(), "");
}

static LLVMValueRef llvm_sizeof(LLVMTypeRef type)
{
   return LLVMBuildIntCast(builder, LLVMSizeOf(type),
                           LLVMInt32Type(), "");
}

static LLVMValueRef llvm_fn(const char *name)
{
   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   if (fn == NULL)
      fatal("cannot find named function %s", name);
   return fn;
}

static void llvm_str(LLVMValueRef *chars, size_t n, const char *str)
{
   for (size_t i = 0; i < n; i++)
      chars[i] = llvm_int8(*str ? *(str++) : '\0');
}

#if 0
static void debug_out(LLVMValueRef val)
{
   LLVMValueRef args[] = { val };
   LLVMBuildCall(builder, llvm_fn("_debug_out"),
                 args, ARRAY_LEN(args), "");
}
#endif

#if 0
static void debug_dump(LLVMValueRef ptr, LLVMValueRef len)
{
   LLVMValueRef args[] = { llvm_void_cast(ptr), len };
   LLVMBuildCall(builder, llvm_fn("_debug_dump"),
                 args, ARRAY_LEN(args), "");
}
#endif

static unsigned bit_width(type_t t)
{
   switch (type_kind(t)) {
   case T_INTEGER:
   case T_PHYSICAL:
      {
         range_t r = type_dim(t, 0);
         uint64_t elements = assume_int(r.right) - assume_int(r.left);

         if (elements <= 2)
            return 1;
         if (elements <= 0xffull)
            return 8;
         else if (elements <= 0xffffull)
            return 16;
         else if (elements <= 0xffffffffull)
            return 32;
         else
            return 64;
      }

   case T_REAL:
       // All real types are doubles at the moment
       return 64;

   case T_SUBTYPE:
      return bit_width(type_base(t));

   case T_ENUM:
      {
         unsigned lits = type_enum_literals(t);

         if (lits <= 2)
            return 1;
         if (lits <= 256)
            return 8;
         else if (lits <= 65356)
            return 16;
         else
            return 32;
      }

   case T_UARRAY:
   case T_CARRAY:
      return bit_width(type_elem(t));

   default:
      assert(false);
   }
}

static LLVMTypeRef llvm_uarray_type(LLVMTypeRef base, int dims)
{
   // Unconstrained arrays are represented by a structure
   // containing the left and right indices, a flag indicating
   // direction, and a pointer to the array data

   LLVMTypeRef dim_fields[] = {
      LLVMInt32Type(),      // Left
      LLVMInt32Type(),      // Right
      LLVMInt8Type()        // Direction
   };

   LLVMTypeRef dim_struct =
      LLVMStructType(dim_fields, ARRAY_LEN(dim_fields), false);

   LLVMTypeRef fields[] = {
      LLVMPointerType(base, 0),
      LLVMArrayType(dim_struct, dims)
   };

   return LLVMStructType(fields, ARRAY_LEN(fields), false);
}

static LLVMTypeRef llvm_type(type_t t)
{
   switch (type_kind(t)) {
   case T_INTEGER:
   case T_PHYSICAL:
   case T_ENUM:
      return LLVMIntType(bit_width(t));

   case T_REAL:
      return LLVMDoubleType();

   case T_SUBTYPE:
      if (!type_is_array(t))
         return llvm_type(type_base(t));

      // Fall-through
   case T_CARRAY:
   case T_UARRAY:
      {
         if (cgen_const_bounds(t)) {
            unsigned nelems = 1;
            for (unsigned i = 0; i < type_dims(t); i++) {
               int64_t low, high;
               range_bounds(type_dim(t, i), &low, &high);
               if (high < low)
                  nelems = 0;
               else
                  nelems *= (high - low + 1);
            }
            return LLVMArrayType(llvm_type(type_elem(t)), nelems);
         }
         else {
            return llvm_uarray_type(llvm_type(type_elem(t)),
                                    cgen_array_dims(t));
         }
      }

   case T_RECORD:
      {
         const char *rec_name = istr(type_ident(t));
         LLVMTypeRef lltype = LLVMGetTypeByName(module, rec_name);
         if (lltype != NULL)
            return lltype;

         lltype = LLVMStructCreateNamed(LLVMGetGlobalContext(), rec_name);
         if (lltype == NULL)
            fatal("failed to add record type %s", rec_name);

         const int nfields = type_fields(t);
         LLVMTypeRef llfields[nfields];

         for (int i = 0; i < nfields; i++) {
            tree_t field = type_field(t, i);
            llfields[i] = llvm_type(tree_type(field));
         }

         LLVMStructSetBody(lltype, llfields, nfields, false);
         return lltype;
      }

   case T_ACCESS:
      return LLVMPointerType(llvm_type(type_access(t)), 0);

   default:
      assert(false);
   }
}

static const char *cgen_mangle_func_name(tree_t decl)
{
   static char buf[512];
   const char *end = buf + sizeof(buf);
   char *p = buf;
   type_t type = tree_type(decl);

   tree_t foreign = tree_attr_tree(decl, foreign_i);
   if (foreign != NULL) {
      assert(tree_kind(foreign) == T_AGGREGATE);

      for (unsigned i = 0; i < tree_assocs(foreign); i++) {
         assoc_t a = tree_assoc(foreign, i);
         assert(a.kind == A_POS);
         assert(tree_kind(a.value) == T_REF);

         tree_t ch = tree_ref(a.value);
         assert(tree_kind(ch) == T_ENUM_LIT);

         *p++ = tree_pos(ch);
      }

      *p = '\0';
   }
   else {
      p += snprintf(p, end - p, "%s", istr(tree_ident(decl)));
      for (unsigned i = 0; i < type_params(type); i++) {
         type_t param = type_param(type, i);
         p += snprintf(p, end - p, "$%s", istr(type_ident(param)));
      }
   }

   return buf;
}

static bool cgen_is_const(tree_t t)
{
   if (tree_kind(t) == T_AGGREGATE) {
      bool is_const = true;
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         assoc_t a = tree_assoc(t, i);
         is_const = is_const && cgen_is_const(a.value);
      }
      return is_const;
   }
   else
      return (tree_kind(t) == T_LITERAL
              || (tree_kind(t) == T_REF
                  && tree_kind(tree_ref(t)) == T_ENUM_LIT));
}

static bool cgen_is_real(type_t type)
{
   return type_kind(type_base_recur(type)) == T_REAL;
}

static bool cgen_const_bounds(type_t type)
{
   if (type_kind(type) == T_UARRAY)
      return false;
   else {
      range_t r = type_dim(type, 0);
      return cgen_is_const(r.left) && cgen_is_const(r.right);
   }
}

static class_t cgen_get_class(tree_t decl)
{
   switch (tree_kind(decl)) {
   case T_VAR_DECL:
      return C_VARIABLE;
   case T_SIGNAL_DECL:
      return C_SIGNAL;
   case T_CONST_DECL:
      return C_CONSTANT;
   default:
      return tree_class(decl);
   }
}

static int cgen_array_dims(type_t type)
{
   assert(type_is_array(type));
   if (type_kind(type) == T_UARRAY)
      return type_index_constrs(type);
   else
      return type_dims(type);
}

static LLVMValueRef cgen_array_meta(type_t type,
                                    LLVMValueRef params[][3],
                                    LLVMValueRef ptr)
{
   // Argument `params' is an array [dims][3] of (left, right, dir)

   LLVMTypeRef base;
   if (type == NULL)   // NULL means generic signal type
      base = cgen_signal_type(NULL);
   else
      base = llvm_type(type_elem(type));

   const int dims = (type == NULL) ? 1 : cgen_array_dims(type);
   LLVMTypeRef uarray_type = llvm_uarray_type(base, dims);

   LLVMTypeRef field_types[LLVMCountStructElementTypes(uarray_type)];
   LLVMGetStructElementTypes(uarray_type, field_types);

   LLVMTypeRef dim_struct = LLVMGetElementType(field_types[1]);

   LLVMValueRef dim_array = LLVMGetUndef(field_types[1]);

   for (int i = 0; i < dims; i++) {
      LLVMValueRef d = LLVMGetUndef(dim_struct);
      d = LLVMBuildInsertValue(builder, d, params[i][0], 0, "");
      d = LLVMBuildInsertValue(builder, d, params[i][1], 1, "");
      d = LLVMBuildInsertValue(builder, d, params[i][2], 2, "");

      dim_array = LLVMBuildInsertValue(builder, dim_array, d, i, "");
   }

   LLVMValueRef var = LLVMGetUndef(uarray_type);
   var = LLVMBuildInsertValue(builder, var, ptr, 0, "");
   var = LLVMBuildInsertValue(builder, var, dim_array, 1, "");

   return var;
}

static LLVMValueRef cgen_array_meta_1(type_t type,
                                      LLVMValueRef left, LLVMValueRef right,
                                      LLVMValueRef kind, LLVMValueRef ptr)
{
   // Special case for one-dimensional array

   assert((type == NULL) || (cgen_array_dims(type) == 1));

   LLVMValueRef params[1][3] = { { left, right, kind } };
   return cgen_array_meta(type, params, ptr);
}

static LLVMValueRef cgen_uarray_dim(LLVMValueRef meta, int dim)
{
   LLVMValueRef dim_array =
         LLVMBuildExtractValue(builder, meta, 1, "dim_array");
   return LLVMBuildExtractValue(builder, dim_array, dim, "dim");
}

static LLVMValueRef cgen_array_dir(type_t type, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      LLVMValueRef dim = cgen_uarray_dim(var, 0 /* XXX */);
      return LLVMBuildExtractValue(builder, dim, 2, "dir");
   }
   else
      return llvm_int8(type_dim(type, 0).kind);
}

static LLVMValueRef cgen_array_left(type_t type, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      LLVMValueRef dim = cgen_uarray_dim(var, 0);
      return LLVMBuildExtractValue(builder, dim, 0, "left");
   }
   else
      return llvm_int32(assume_int(type_dim(type, 0).left));
}

static LLVMValueRef cgen_array_right(type_t type, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      LLVMValueRef dim = cgen_uarray_dim(var, 0 /* XXX */);
      return LLVMBuildExtractValue(builder, dim, 1, "right");
   }
   else
      return llvm_int32(assume_int(type_dim(type, 0).right));
}

static LLVMValueRef cgen_array_len(type_t type, int dim, LLVMValueRef data)
{
   // Passing dimension -1 means sum over all dimensions

   if (cgen_const_bounds(type)) {
      int n_elems = 1;
      const int ndims = type_dims(type);
      for (int i = 0; i < ndims; i++) {
         if ((dim == -1) || (i == dim)) {
            int64_t low, high;
            range_bounds(type_dim(type, i), &low, &high);

            if (high < low)
               n_elems = 0;
            else
               n_elems *= (high - low + 1);
         }
      }
      return llvm_int32(n_elems);
   }
   else {
      LLVMValueRef n_elems = llvm_int32(1);
      const int ndims = cgen_array_dims(type);
      for (int i = 0; i < ndims; i++) {
         if ((dim == -1) || (i == dim)) {
            LLVMValueRef dim_struct = cgen_uarray_dim(data, i);

            LLVMValueRef downto = LLVMBuildICmp(
               builder, LLVMIntEQ,
               LLVMBuildExtractValue(builder, dim_struct, 2, "dir"),
               llvm_int8(RANGE_DOWNTO),
               "downto");
            LLVMValueRef left =
               LLVMBuildExtractValue(builder, dim_struct, 0, "left");
            LLVMValueRef right =
               LLVMBuildExtractValue(builder, dim_struct, 1, "right");
            LLVMValueRef diff  =
               LLVMBuildSelect(builder, downto,
                               LLVMBuildSub(builder, left, right, ""),
                               LLVMBuildSub(builder, right, left, ""),
                               "diff");
            LLVMValueRef len =
               LLVMBuildAdd(builder, diff, llvm_int32(1), "len");
            LLVMValueRef neg = LLVMBuildICmp(builder, LLVMIntSLT, len,
                                             llvm_int32(0), "negative");
            LLVMValueRef clamp =
               LLVMBuildSelect(builder, neg, llvm_int32(0), len, "len_clamp");

            n_elems = LLVMBuildMul(builder, n_elems, clamp, "n_elems");
         }
      }

      return n_elems;
   }
}

static LLVMValueRef cgen_array_len_recur(type_t type, LLVMValueRef data)
{
   LLVMValueRef n_elems = llvm_int32(1);
   type_t elem = type;
   do {
      LLVMValueRef dim_len = cgen_array_len(elem, -1, data);
      n_elems = LLVMBuildMul(builder, n_elems, dim_len, "");
   } while (type_is_array((elem = type_elem(elem))));

   return n_elems;
}

static LLVMValueRef cgen_array_elem_size(type_t type)
{
   while (type_is_array((type = type_elem(type))))
      ;
   return llvm_sizeof(llvm_type(type));
}

static LLVMValueRef cgen_tmp_var(type_t type, const char *name, cgen_ctx_t *ctx)
{
   // Handle case where array size is not known until run time
   if (type_is_array(type) && !cgen_const_bounds(type)) {
      const int dims = cgen_array_dims(type);
      LLVMValueRef params[dims][3];

      LLVMValueRef size = llvm_int32(1);

      for (int i = 0; i < dims; i++) {
         range_t r = type_dim(type, i);

         LLVMValueRef kind_ll;
         if (r.kind == RANGE_DYN) {
            // This can only appear when using 'RANGE
            assert(tree_kind(r.left) == T_FCALL);
            param_t p = tree_param(r.left, 0);
            assert(tree_kind(p.value) == T_REF);

            LLVMValueRef uarray;
            tree_t decl = tree_ref(p.value);
            if (cgen_get_class(decl) == C_SIGNAL) {
               uarray = tree_attr_ptr(decl, sig_struct_i);
               assert(uarray != NULL);
            }
            else
               uarray = cgen_get_var(decl, ctx);

            kind_ll = cgen_array_dir(type, uarray);
         }
         else
            kind_ll = llvm_int8(r.kind);

         LLVMValueRef downto =
            LLVMBuildICmp(builder, LLVMIntEQ,
                          kind_ll, llvm_int8(RANGE_DOWNTO), "downto");

         LLVMValueRef left  = cgen_expr(r.left, ctx);
         LLVMValueRef right = cgen_expr(r.right, ctx);

         LLVMValueRef diff =
            LLVMBuildSelect(builder, downto,
                            LLVMBuildSub(builder, left, right, ""),
                            LLVMBuildSub(builder, right, left, ""), "");

         params[i][0] = left;
         params[i][1] = right;
         params[i][2] = kind_ll;

         LLVMValueRef length =
            LLVMBuildAdd(builder, diff, llvm_int32(1), "length");
         size = LLVMBuildMul(builder, size, length, "size");
      }

      LLVMTypeRef base_type = llvm_type(type_elem(type));
      LLVMTypeRef ptr_type  = LLVMPointerType(base_type, 0);

      LLVMValueRef buf =
         LLVMBuildArrayAlloca(builder, base_type, size, "buf");

      LLVMValueRef ptr = LLVMBuildPointerCast(builder, buf, ptr_type, "");

      LLVMValueRef meta = cgen_array_meta(type, params, ptr);
      LLVMSetValueName(meta, name);
      return meta;
   }
   else {
      LLVMValueRef var = LLVMBuildAlloca(builder, llvm_type(type), name);

      if (type_is_array(type)) {
         // Get a pointer to the first element
         LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
         return LLVMBuildGEP(builder, var, indexes, ARRAY_LEN(indexes), "");
      }
      else
         return var;
   }
}

static LLVMValueRef cgen_array_off(LLVMValueRef off, LLVMValueRef array,
                                   type_t type, cgen_ctx_t *ctx,
                                   unsigned dim)
{
   // Convert VHDL offset 'off' to a zero-based LLVM array offset

   LLVMValueRef low;
   if (!cgen_const_bounds(type)) {
      assert(array != NULL);

      LLVMValueRef dim_struct = cgen_uarray_dim(array, dim);

      LLVMValueRef dir =
         LLVMBuildExtractValue(builder, dim_struct, 2, "dir");
      LLVMValueRef is_downto =
         LLVMBuildICmp(builder, LLVMIntEQ, dir,
                       llvm_int8(RANGE_DOWNTO), "is_downto");
      LLVMValueRef left =
         LLVMBuildExtractValue(builder, dim_struct, 0, "left");
      LLVMValueRef right =
         LLVMBuildExtractValue(builder, dim_struct, 1, "right");
      low = LLVMBuildSelect(builder, is_downto, right, left, "low");
   }
   else {
      range_t r = type_dim(type, dim);
      low = cgen_expr(r.kind == RANGE_TO ? r.left : r.right, ctx);
   }

   LLVMValueRef zero_based = LLVMBuildSub(builder, off, low, "");

   // Array offsets are always 32-bit
   return LLVMBuildZExt(builder, zero_based, LLVMInt32Type(), "");
}
static void cgen_check_bounds(tree_t t, LLVMValueRef kind, LLVMValueRef value,
                              LLVMValueRef min, LLVMValueRef max,
                              cgen_ctx_t *ctx)
{
   LLVMValueRef value32 = LLVMBuildZExt(builder, value,
                                        LLVMInt32Type(), "value32");

   LLVMValueRef above =
      LLVMBuildICmp(builder, LLVMIntUGE, value32, min, "above");
   LLVMValueRef below =
      LLVMBuildICmp(builder, LLVMIntULE, value32, max, "below");

   LLVMValueRef in = LLVMBuildAnd(builder, above, below, "in");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, in, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   LLVMValueRef args[] = {
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      value32,
      min,
      max,
      kind
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_check_array_bounds(tree_t t, type_t type, LLVMValueRef array,
                                    LLVMValueRef value, cgen_ctx_t *ctx)
{
   LLVMValueRef left  = cgen_array_left(type, array);
   LLVMValueRef right = cgen_array_right(type, array);
   LLVMValueRef dir   = cgen_array_dir(type, array);

   LLVMValueRef to = LLVMBuildICmp(builder, LLVMIntEQ, dir,
                                   llvm_int8(RANGE_TO), "to");

   LLVMValueRef min = LLVMBuildSelect(builder, to, left, right, "min");
   LLVMValueRef max = LLVMBuildSelect(builder, to, right, left, "max");

   LLVMValueRef kind = LLVMBuildSelect(builder, to,
                                       llvm_int32(BOUNDS_ARRAY_TO),
                                       llvm_int32(BOUNDS_ARRAY_DOWNTO),
                                       "kind");

   cgen_check_bounds(t, kind, value, min, max, ctx);
}

static LLVMValueRef cgen_get_slice(LLVMValueRef array, type_t type,
                                   range_t r, cgen_ctx_t *ctx)
{
   // Construct a new array sharing the same memory as `array' but offset
   // by `range'

   assert(type_is_array(type));

   LLVMValueRef left  = cgen_expr(r.left, ctx);
   LLVMValueRef right = cgen_expr(r.right, ctx);

   LLVMValueRef low  = (r.kind == RANGE_TO) ? left : right;
   LLVMValueRef high = (r.kind == RANGE_TO) ? right : left;

   LLVMValueRef null = LLVMBuildICmp(builder, LLVMIntSLT, high, low, "null");

   LLVMBasicBlockRef check_bb = LLVMAppendBasicBlock(ctx->fn, "check");
   LLVMBasicBlockRef merge_bb = LLVMAppendBasicBlock(ctx->fn, "merge");

   LLVMBuildCondBr(builder, null, merge_bb, check_bb);

   LLVMPositionBuilderAtEnd(builder, check_bb);

   cgen_check_array_bounds(r.left, type, array, left, ctx);
   cgen_check_array_bounds(r.right, type, array, right, ctx);

   LLVMBuildBr(builder, merge_bb);

   LLVMPositionBuilderAtEnd(builder, merge_bb);

   LLVMValueRef off = cgen_array_off(low, array, type, ctx, 0);
   LLVMValueRef data = cgen_array_data_ptr(type, array);

   LLVMTypeRef ptr_type = LLVMPointerType(llvm_type(type_elem(type)), 0);

   LLVMValueRef ptr_norm = LLVMBuildGEP(builder, data, &off, 1, "");
   LLVMValueRef ptr_null = LLVMConstNull(ptr_type);

   LLVMValueRef ptr = LLVMBuildSelect(builder, null, ptr_null, ptr_norm, "ptr");

   bool unwrap = cgen_is_const(r.left) && cgen_is_const(r.right);

   if (unwrap)
      return ptr;
   else
      return cgen_array_meta_1(type, left, right, llvm_int8(r.kind), ptr);
}

static LLVMValueRef cgen_array_signal_ptr(tree_t decl, LLVMValueRef elem)
{
   LLVMValueRef indexes[] = { llvm_int32(0), elem };
   return LLVMBuildGEP(builder, tree_attr_ptr(decl, sig_struct_i),
                       indexes, ARRAY_LEN(indexes), "");
}

static LLVMValueRef cgen_get_var(tree_t decl, cgen_ctx_t *ctx)
{
   void *local = tree_attr_ptr(decl, local_var_i);
   if (local != NULL)
      return (LLVMValueRef)local;

   void *global = tree_attr_ptr(decl, global_const_i);
   if (global != NULL)
      return LLVMBuildLoad(builder, (LLVMValueRef)global, "global");

   tree_kind_t kind = tree_kind(decl);
   assert((kind == T_VAR_DECL) || (kind == T_CONST_DECL));

   type_t type = tree_type(decl);

   int offset = tree_attr_int(decl, var_offset_i, -1);
   if (offset == -1) {
      // This variable is not defined anywhere in this translation unit
      // so make it an external symbol and hope the linker fixes it up
      LLVMTypeRef lltype = llvm_type(type);
      LLVMValueRef var = LLVMAddGlobal(module, lltype, istr(tree_ident(decl)));
      LLVMSetLinkage(var, LLVMExternalLinkage);

      tree_add_attr_ptr(decl, local_var_i, var);
      return var;
   }

   LLVMValueRef var = LLVMBuildStructGEP(builder, ctx->state, offset, "");
   if (type_is_array(type)) {
      if (cgen_const_bounds(type)) {
         // Get a pointer to the first element
         LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
         return LLVMBuildGEP(builder, var, indexes, ARRAY_LEN(indexes), "");
      }
      else {
         // Load the meta data
         return LLVMBuildLoad(builder, var, "meta");
      }
   }
   else
      return var;
}

static const char *cgen_memcpy_name(int width)
{
   static char name[64];
   snprintf(name, sizeof(name),
            "llvm.memcpy.p0i%d.p0i%d.i32", width, width);
   return name;
}

static void cgen_array_copy(type_t src_type, type_t dest_type,
                            LLVMValueRef src, LLVMValueRef dst,
                            LLVMValueRef offset, cgen_ctx_t *ctx)
{
   LLVMValueRef src_dir = cgen_array_dir(src_type, src);
   LLVMValueRef dst_dir = cgen_array_dir(dest_type, dst);

   LLVMValueRef ll_n_elems = cgen_array_len(src_type, -1, src);

   if (!cgen_const_bounds(dest_type))
      dst = cgen_array_data_ptr(dest_type, dst);

   LLVMValueRef src_ptr = cgen_array_data_ptr(src_type, src);

   if (offset == NULL)
      offset = llvm_int32(0);

   LLVMValueRef indexes[] = { offset };
   LLVMValueRef dst_ptr = LLVMBuildGEP(builder, dst, indexes,
                                       ARRAY_LEN(indexes), "dst_ptr");

   LLVMValueRef opposite_dir =
      LLVMBuildICmp(builder, LLVMIntNE, src_dir, dst_dir, "opp_dir");

   bool same_dir_const =
      (cgen_const_bounds(src_type) && cgen_const_bounds(dest_type)
       && (type_dim(src_type, 0).kind == type_dim(dest_type, 0).kind));

   type_t elem_type = type_elem(src_type);

   int align, width;
   LLVMValueRef bytes;
   if (type_kind(elem_type) == T_RECORD) {
      width = 8;
      align = 1;
      bytes = LLVMBuildIntCast(builder, LLVMSizeOf(llvm_type(elem_type)),
                               LLVMInt32Type(), "");

      src_ptr = llvm_void_cast(src_ptr);
      dst_ptr = llvm_void_cast(dst_ptr);
   }
   else {
      width = bit_width(src_type);
      const int b = (width / 8) + ((width % 8 > 0) ? 1 : 0);
      bytes = llvm_int32(b);
      align = b;
   }

   // Cast real values to 64-bit integers
   const bool real = cgen_is_real(elem_type);
   if (real) {
      LLVMTypeRef pi64 = LLVMPointerType(LLVMInt64Type(), 0);
      src_ptr = LLVMBuildPointerCast(builder, src_ptr, pi64, "src_pi64");
      dst_ptr = LLVMBuildPointerCast(builder, dst_ptr, pi64, "dst_pi64");
   }

   LLVMValueRef size = LLVMBuildMul(builder, ll_n_elems, bytes, "size");

   LLVMValueRef memcpy_args[] = {
      dst_ptr,
      src_ptr,
      size,
      llvm_int32(align),
      llvm_int1(0)
   };

   LLVMValueRef memcpy_fn = llvm_fn(cgen_memcpy_name(width));

   if (same_dir_const) {
      // Fast path: use LLVM memcpy
      LLVMBuildCall(builder, memcpy_fn,
                    memcpy_args, ARRAY_LEN(memcpy_args), "");
   }
   else {
      LLVMBasicBlockRef fast_bb = LLVMAppendBasicBlock(ctx->fn, "fast");
      LLVMBasicBlockRef slow_bb = LLVMAppendBasicBlock(ctx->fn, "slow");
      LLVMBasicBlockRef done_bb = LLVMAppendBasicBlock(ctx->fn, "done");

      LLVMBuildCondBr(builder, opposite_dir, slow_bb, fast_bb);

      // Fast(ish) path: call LLVM memcpy after condition

      LLVMPositionBuilderAtEnd(builder, fast_bb);
      LLVMBuildCall(builder, memcpy_fn,
                    memcpy_args, ARRAY_LEN(memcpy_args), "");
      LLVMBuildBr(builder, done_bb);

      // Slow path: call _array_reverse helper function

      LLVMPositionBuilderAtEnd(builder, slow_bb);

      LLVMValueRef reverse_args[] = {
         llvm_void_cast(dst),
         llvm_void_cast(src_ptr),
         offset,
         ll_n_elems,
         llvm_sizeof(llvm_type(elem_type))
      };
      LLVMBuildCall(builder, llvm_fn("_array_reverse"),
                    reverse_args, ARRAY_LEN(reverse_args), "");

      LLVMBuildBr(builder, done_bb);

      LLVMPositionBuilderAtEnd(builder, done_bb);
   }
}

static LLVMValueRef cgen_local_var(tree_t d, cgen_ctx_t *ctx)
{
   type_t type = tree_type(d);
   LLVMValueRef var = cgen_tmp_var(type, istr(tree_ident(d)), ctx);

   if (tree_has_value(d)) {
      if (type_is_array(type)) {
         LLVMValueRef init = cgen_expr(tree_value(d), ctx);
         cgen_array_copy(type, type, init, var, NULL, ctx);
      }
      else
         LLVMBuildStore(builder, cgen_expr(tree_value(d), ctx), var);
   }

   return var;
}

static void cgen_prototype(tree_t t, LLVMTypeRef *args, bool procedure)
{
   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      type_t type = tree_type(p);

      port_mode_t mode = tree_port_mode(p);
      const bool array = type_is_array(type);

      switch (tree_class(p)) {
      case C_SIGNAL:
         {
            LLVMTypeRef base_type = cgen_signal_type(type);
            args[i] = array ? base_type : LLVMPointerType(base_type, 0);
         }
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
         {
            bool need_ptr = ((mode == PORT_OUT || mode == PORT_INOUT)
                             && !array);
            if (need_ptr)
               args[i] = LLVMPointerType(llvm_type(type), 0);
            else if (array && (type_kind(type) != T_UARRAY))
               args[i] = LLVMPointerType(llvm_type(type), 0);
            else
               args[i] = llvm_type(type);
         }
         break;

      default:
         assert(false);
      }
   }

   if (procedure) {
      // Last parameter is context
      args[nports] = llvm_void_ptr();
   }
}

static LLVMValueRef cgen_fdecl(tree_t t)
{
   const char *mangled = cgen_mangle_func_name(t);
   LLVMValueRef fn = LLVMGetNamedFunction(module, mangled);
   if (fn != NULL)
      return fn;
   else {
      type_t ftype = tree_type(t);

      LLVMTypeRef atypes[tree_ports(t)];
      cgen_prototype(t, atypes, false);

      return LLVMAddFunction(
         module,
         cgen_mangle_func_name(t),
         LLVMFunctionType(llvm_type(type_result(ftype)),
                          atypes,
                          type_params(ftype),
                          false));
   }
}

static LLVMValueRef cgen_pdecl(tree_t t)
{
   const char *mangled = cgen_mangle_func_name(t);
   LLVMValueRef fn = LLVMGetNamedFunction(module, mangled);
   if (fn != NULL)
      return fn;
   else {
      const int nports = tree_ports(t);
      LLVMTypeRef atypes[nports + 1];
      cgen_prototype(t, atypes, true);

      return LLVMAddFunction(
         module,
         cgen_mangle_func_name(t),
         LLVMFunctionType(llvm_void_ptr(),
                          atypes,
                          nports + 1,
                          false));
   }
}

static LLVMValueRef cgen_literal(tree_t t)
{
   literal_t l = tree_literal(t);
   LLVMTypeRef lltype = llvm_type(tree_type(t));
   switch (l.kind) {
   case L_INT:
      return LLVMConstInt(lltype, l.i, false);
   case L_REAL:
      return LLVMConstReal(lltype, l.r);
   case L_NULL:
      return LLVMConstNull(lltype);
   default:
      assert(false);
   }
}

static LLVMValueRef cgen_vec_load(LLVMValueRef signal, type_t type,
                                  type_t slice_type, bool last_value,
                                  cgen_ctx_t *ctx)
{
   // Copy the resolved signal into a temporary array

   const bool src_uarray = !cgen_const_bounds(type);
   const bool dst_uarray = !cgen_const_bounds(slice_type);

   LLVMValueRef fn = llvm_fn("_vec_load");

   LLVMValueRef left  = cgen_array_left(slice_type, signal);
   LLVMValueRef right = cgen_array_right(slice_type, signal);
   LLVMValueRef dir   = cgen_array_dir(slice_type, signal);

   LLVMValueRef dir_to =
      LLVMBuildICmp(builder, LLVMIntEQ, dir, llvm_int8(RANGE_TO), "to");

   LLVMValueRef low  = LLVMBuildSelect(builder, dir_to, left, right, "");
   LLVMValueRef high = LLVMBuildSelect(builder, dir_to, right, left, "");

   LLVMValueRef low_abs  = cgen_array_off(low, signal, type, ctx, 0);
   LLVMValueRef high_abs = cgen_array_off(high, signal, type, ctx, 0);

   LLVMValueRef tmp;
   if (dst_uarray) {
      LLVMValueRef length =
         LLVMBuildAdd(builder,
                      LLVMBuildSub(builder, high, low, ""),
                      llvm_int32(1),
                      "length");
      tmp = LLVMBuildArrayAlloca(builder, llvm_type(type_elem(type)),
                                 length, "tmp");
   }
   else
      tmp = LLVMBuildAlloca(builder, llvm_type(slice_type), "tmp");

   LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };

   LLVMValueRef p_signal =
      src_uarray
      ? LLVMBuildExtractValue(builder, signal, 0, "")
      : LLVMBuildGEP(builder, signal, indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef p_tmp =
      dst_uarray
      ? tmp
      : LLVMBuildGEP(builder, tmp, indexes, ARRAY_LEN(indexes), "");

   int width = bit_width(type_elem(type));
   int bytes = (width / 8) + ((width % 8 > 0) ? 1 : 0);

   LLVMValueRef args[] = {
      llvm_void_cast(p_signal),
      llvm_void_cast(p_tmp),
      llvm_int32(bytes),
      low_abs,
      high_abs,
      llvm_int1(last_value)
   };
   LLVMBuildCall(builder, fn, args, ARRAY_LEN(args), "");

   if (dst_uarray)
      return cgen_array_meta_1(slice_type, left, right, dir, tmp);
   else
      return tmp;
}

static LLVMValueRef cgen_sig_struct(tree_t decl)
{
   // Return the signal structure for a signal declaration
   return tree_attr_ptr(decl, sig_struct_i);
}

static LLVMValueRef cgen_scalar_signal_flag(tree_t signal, int flag)
{
   LLVMValueRef signal_struct = tree_attr_ptr(signal, sig_struct_i);
   LLVMValueRef bit = llvm_int8(flag);
   LLVMValueRef ptr =
      LLVMBuildStructGEP(builder, signal_struct, SIGNAL_FLAGS, "");
   LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
   LLVMValueRef masked = LLVMBuildAnd(builder, deref, bit, "");
   return LLVMBuildICmp(builder, LLVMIntEQ, masked, bit, "");
}

static LLVMValueRef cgen_array_signal_flag(tree_t signal, int flag)
{
   // Need to OR the flag for each sub-element

   LLVMValueRef bit = llvm_int8(flag);

   int64_t low, high;
   range_bounds(type_dim(tree_type(signal), 0), &low, &high);

   LLVMValueRef result = llvm_int8(0);
   for (int i = 0; i < high - low + 1; i++) {
      LLVMValueRef struct_ptr =
         cgen_array_signal_ptr(signal, llvm_int32(i));
      LLVMValueRef flags_ptr =
         LLVMBuildStructGEP(builder, struct_ptr, SIGNAL_FLAGS, "");
      LLVMValueRef deref = LLVMBuildLoad(builder, flags_ptr, "");
      LLVMValueRef masked = LLVMBuildAnd(builder, deref, bit, "");
      result = LLVMBuildOr(builder, result, masked, "");
   }

   return LLVMBuildICmp(builder, LLVMIntEQ, result, bit, "");
}

static LLVMValueRef cgen_signal_flag(tree_t ref, int flag)
{
   tree_t sig_decl = tree_ref(ref);
   if (type_kind(tree_type(sig_decl)) == T_CARRAY)
      return cgen_array_signal_flag(sig_decl, flag);
   else
      return cgen_scalar_signal_flag(sig_decl, flag);
}

static LLVMValueRef cgen_last_value(tree_t signal, cgen_ctx_t *ctx)
{
   tree_t sig_decl = tree_ref(signal);

   if (type_kind(tree_type(sig_decl)) == T_CARRAY) {
      type_t type = tree_type(sig_decl);
      return cgen_vec_load(cgen_sig_struct(sig_decl), type, type, true, ctx);
   }
   else {
      LLVMValueRef signal = tree_attr_ptr(sig_decl, sig_struct_i);
      LLVMValueRef ptr =
         LLVMBuildStructGEP(builder, signal, SIGNAL_LAST_VALUE, "");
      LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
      return LLVMBuildIntCast(builder, deref,
                              llvm_type(tree_type(sig_decl)), "");
   }
}

static LLVMValueRef cgen_uarray_field(tree_t ref, int field, cgen_ctx_t *ctx)
{
   assert(tree_kind(ref) == T_REF);
   tree_t decl = tree_ref(ref);

   LLVMValueRef meta;
   if (cgen_get_class(decl) == C_SIGNAL) {
      meta = tree_attr_ptr(decl, sig_struct_i);
      assert(meta != NULL);
   }
   else
      meta = cgen_get_var(decl, ctx);

   LLVMValueRef dim_struct = cgen_uarray_dim(meta, 0 /* XXX */);

   return LLVMBuildExtractValue(builder, dim_struct, field, "");
}

static LLVMValueRef cgen_uarray_asc(tree_t ref, cgen_ctx_t *ctx)
{
   return LLVMBuildICmp(
      builder, LLVMIntEQ,
      cgen_uarray_field(ref, 2, ctx),
      llvm_int8(RANGE_TO),
      "ascending");
}

static void cgen_call_args(tree_t t, LLVMValueRef *args, type_t *arg_types,
                           cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   ident_t builtin = tree_attr_str(decl, ident_new("builtin"));

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   for (int i = 0; i < nparams; i++) {
      param_t p = tree_param(t, i);
      type_t type = tree_type(p.value);
      class_t class = (builtin == NULL)
         ? tree_class(tree_port(decl, i))
         : C_DEFAULT;

      arg_types[i] = type;

      if ((builtin == NULL) && (class == C_SIGNAL)) {
         // Pass a pointer to the global signal structure
         assert(tree_kind(p.value) == T_REF);
         tree_t sig_decl = tree_ref(p.value);
         args[i] = tree_attr_ptr(sig_decl, sig_struct_i);
         assert(args[i] != NULL);
      }
      else {
         args[i] = NULL;

         // If this is a scalar out or inout parameter then we need
         // to pass a pointer rather than the value
         if ((builtin == NULL) || (i < nports)) {
            tree_t port = tree_port(decl, i);
            port_mode_t mode = tree_port_mode(port);
            bool need_ptr = ((mode == PORT_OUT || mode == PORT_INOUT)
                             && !type_is_array(type));
            if (need_ptr)
               args[i] = cgen_var_lvalue(p.value, ctx);
         }

         if (args[i] == NULL)
            args[i] = cgen_expr(p.value, ctx);
      }

      type_t formal_type;
      if ((builtin == NULL) || (i < nports))
         formal_type = tree_type(tree_port(decl, i));
      else
         formal_type = tree_type(p.value);

      // If we are passing a constrained array argument wrap it in
      // a structure with its metadata. Note we don't need to do
      // this for unconstrained arrays as they are already wrapped.
      bool need_wrap =
         (type_kind(formal_type) == T_UARRAY)
         && cgen_const_bounds(type)
         && (builtin == NULL);

      if (need_wrap) {
         LLVMValueRef data;
         if (class == C_SIGNAL) {
            LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
            data = LLVMBuildGEP(builder, args[i],
                                indexes, ARRAY_LEN(indexes), "");
         }
         else {
            LLVMTypeRef ptr_type =
               LLVMPointerType(llvm_type(type_elem(type)), 0);
            data = LLVMBuildPointerCast(builder, args[i], ptr_type, "");
         }

         const int dims = cgen_array_dims(type);
         LLVMValueRef params[dims][3];
         for (int i = 0; i < dims; i++) {
            range_t r = type_dim(type, i);
            params[i][0] = llvm_int32(assume_int(r.left));
            params[i][1] = llvm_int32(assume_int(r.right));
            params[i][2] = llvm_int8(r.kind);
         }

         args[i] = cgen_array_meta(
            (class == C_SIGNAL) ? NULL : type,
            params,
            data);
      }

      // If we are passing an unconstrained array actual to a
      // constrained formal then we need to unwrap the array
      if ((type_kind(formal_type) == T_CARRAY) && (builtin == NULL)) {
         LLVMValueRef ptr = args[i];
         if (!cgen_const_bounds(type))
            ptr = LLVMBuildExtractValue(builder, args[i], 0, "aptr");

         LLVMTypeRef lt = LLVMPointerType(llvm_type(formal_type), 0);
         args[i] = LLVMBuildPointerCast(builder, ptr, lt, "");
      }
   }
}

static LLVMValueRef cgen_array_rel(LLVMValueRef lhs, LLVMValueRef rhs,
                                   type_t left_type, type_t right_type,
                                   LLVMIntPredicate pred, cgen_ctx_t *ctx)
{
   // Behaviour of relational operators on arrays is described in
   // LRM 93 section 7.2.2

   LLVMValueRef left_len  = cgen_array_len(left_type, 0, lhs);
   LLVMValueRef right_len = cgen_array_len(right_type, 0, rhs);

   LLVMValueRef left_base  = cgen_array_data_ptr(left_type, lhs);
   LLVMValueRef right_base = cgen_array_data_ptr(right_type, rhs);

   LLVMValueRef ldir = cgen_array_dir(left_type, lhs);
   LLVMValueRef rdir = cgen_array_dir(right_type, rhs);

   LLVMValueRef l_downto = LLVMBuildICmp(builder, LLVMIntEQ, ldir,
                                         llvm_int8(RANGE_DOWNTO), "l_downto");
   LLVMValueRef r_downto = LLVMBuildICmp(builder, LLVMIntEQ, rdir,
                                         llvm_int8(RANGE_DOWNTO), "r_downto");

   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, llvm_int32(0), i);

   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "rel_test");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "rel_body");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "rel_exit");

   LLVMBuildBr(builder, test_bb);

   // Loop test

   LLVMPositionBuilderAtEnd(builder, test_bb);

   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i");
   LLVMValueRef len_ge_l = LLVMBuildICmp(builder, LLVMIntUGE, i_loaded,
                                         left_len, "len_ge_l");
   LLVMValueRef len_ge_r = LLVMBuildICmp(builder, LLVMIntUGE, i_loaded,
                                         right_len, "len_ge_r");
   LLVMBuildCondBr(builder, LLVMBuildOr(builder, len_ge_l, len_ge_r, ""),
                   exit_bb, body_bb);

   // Loop body

   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef i_plus_1   = LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "");
   LLVMValueRef l_off_down = LLVMBuildSub(builder, left_len, i_plus_1, "");
   LLVMValueRef r_off_down = LLVMBuildSub(builder, right_len, i_plus_1, "");

   LLVMValueRef l_off = LLVMBuildSelect(builder, l_downto,
                                        l_off_down, i_loaded, "l_off");
   LLVMValueRef r_off = LLVMBuildSelect(builder, r_downto,
                                        r_off_down, i_loaded, "r_off");

   LLVMValueRef l_ptr = LLVMBuildGEP(builder, left_base,
                                     &l_off, 1, "l_ptr");
   LLVMValueRef r_ptr = LLVMBuildGEP(builder, right_base,
                                     &r_off, 1, "r_ptr");

   LLVMValueRef l_val = LLVMBuildLoad(builder, l_ptr, "l_val");
   LLVMValueRef r_val = LLVMBuildLoad(builder, r_ptr, "r_val");

   LLVMValueRef cmp = LLVMBuildICmp(builder, pred, l_val, r_val, "cmp");
   LLVMValueRef eq  = (pred == LLVMIntEQ) ? cmp
      : LLVMBuildICmp(builder, LLVMIntEQ, l_val, r_val, "eq");

   LLVMValueRef inc =
      LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "inc");
   LLVMBuildStore(builder, inc, i);

   LLVMBuildCondBr(builder, eq, test_bb, exit_bb);

   // Epilogue

   LLVMPositionBuilderAtEnd(builder, exit_bb);

   LLVMValueRef phi = LLVMBuildPhi(builder, LLVMInt1Type(), "arel");

   LLVMValueRef      values[] = { cmp,     len_ge_l };
   LLVMBasicBlockRef bbs[]    = { body_bb, test_bb  };
   LLVMAddIncoming(phi, values, bbs, 2);

   return phi;
}

static LLVMValueRef cgen_agg_bound(tree_t t, bool low, int32_t def,
                                   cgen_ctx_t *ctx)
{
   assert(tree_kind(t) == T_AGGREGATE);

   LLVMValueRef result = llvm_int32(def);

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);
      LLVMValueRef this = NULL;
      switch (a.kind) {
         case A_NAMED:
            this = cgen_expr(a.name, ctx);
            break;

         case A_RANGE:
            if ((low && a.range.kind == RANGE_TO)
                || (!low && a.range.kind == RANGE_DOWNTO))
               this = cgen_expr(a.range.left, ctx);
            else
               this = cgen_expr(a.range.right, ctx);
            break;

         default:
            assert(false);
      }

      LLVMIntPredicate pred = (low ? LLVMIntSLT : LLVMIntSGT);
      LLVMValueRef cmp = LLVMBuildICmp(builder, pred, this, result, "");
      result = LLVMBuildSelect(builder, cmp, this, result, "");
   }

   return result;
}

static LLVMValueRef cgen_name_attr(tree_t ref, int which)
{
   tree_t decl = tree_ref(ref);

   LLVMValueRef signal = tree_attr_ptr(decl, sig_struct_i);
   LLVMValueRef res = LLVMBuildAlloca(builder,
                                      llvm_uarray_type(LLVMInt8Type(), 1),
                                      "name_attr");
   LLVMValueRef args[] = {
      llvm_void_cast(signal),
      llvm_int32(which),
      res,
   };
   LLVMBuildCall(builder, llvm_fn("_name_attr"), args, ARRAY_LEN(args), "");
   return LLVMBuildLoad(builder, res, "");
}

static LLVMValueRef cgen_record_eq(LLVMValueRef left, LLVMValueRef right,
                                   type_t left_type, type_t right_type,
                                   LLVMIntPredicate pred, cgen_ctx_t *ctx)
{
   // We need to get the address of the records to pass to memcmp so
   // copy them into temporaries on the stack: this should be optimised
   // away by LLVM

   LLVMValueRef left_tmp =
      LLVMBuildAlloca(builder, llvm_type(left_type), "left_tmp");
   LLVMValueRef right_tmp =
      LLVMBuildAlloca(builder, llvm_type(right_type), "right_tmp");

   LLVMBuildStore(builder, left, left_tmp);
   LLVMBuildStore(builder, right, right_tmp);

   LLVMValueRef args[] = {
      llvm_void_cast(left_tmp),
      llvm_void_cast(right_tmp),
      llvm_sizeof(llvm_type(left_type))
   };
   LLVMValueRef cmp = LLVMBuildCall(builder, llvm_fn("memcmp"),
                                    args, ARRAY_LEN(args), "cmp");

   return LLVMBuildICmp(builder, pred, cmp, llvm_int32(0), "");
}

static LLVMValueRef cgen_last_event(tree_t t)
{
   LLVMValueRef signal_struct = tree_attr_ptr(tree_ref(t), sig_struct_i);
   LLVMValueRef ptr =
      LLVMBuildStructGEP(builder, signal_struct, SIGNAL_LAST_EVENT, "");
   return LLVMBuildLoad(builder, ptr, "");
}

static void cgen_promote_arith(type_t result, LLVMValueRef *args, int nargs)
{
   // Ensure all arguments to arithmetic operators are the same width

   LLVMTypeRef lltype = llvm_type(result);
   unsigned width = LLVMGetIntTypeWidth(lltype);

   for (int i = 0; i < nargs; i++) {
      LLVMTypeRef arg_type = LLVMTypeOf(args[i]);
      if (LLVMGetIntTypeWidth(arg_type) != width)
         args[i] = LLVMBuildIntCast(builder, args[i], lltype, "promote");
   }
}

static LLVMValueRef cgen_fcall(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   ident_t builtin = tree_attr_str(decl, ident_new("builtin"));

   // Special attributes
   if (builtin) {
      tree_t p0 = tree_param(t, 0).value;

      if (icmp(builtin, "event"))
         return cgen_signal_flag(p0, SIGNAL_F_EVENT);
      else if (icmp(builtin, "active"))
         return cgen_signal_flag(p0, SIGNAL_F_ACTIVE);
      else if (icmp(builtin, "last_value"))
         return cgen_last_value(p0, ctx);
      else if (icmp(builtin, "agg_low"))
         return cgen_agg_bound(p0, true, INT32_MAX, ctx);
      else if (icmp(builtin, "agg_high"))
         return cgen_agg_bound(p0, false, INT32_MIN, ctx);
      else if (icmp(builtin, "instance_name"))
         return cgen_name_attr(p0, 1);
      else if (icmp(builtin, "path_name"))
         return cgen_name_attr(p0, 0);
      else if (icmp(builtin, "last_event"))
         return cgen_last_event(p0);
      else if (icmp(builtin, "uarray_left"))
         return cgen_uarray_field(p0, 0, ctx);
      else if (icmp(builtin, "uarray_right"))
         return cgen_uarray_field(p0, 1, ctx);
      else if (icmp(builtin, "uarray_asc"))
         return cgen_uarray_asc(p0, ctx);
      else if (icmp(builtin, "uarray_low")) {
         return LLVMBuildSelect(
            builder, cgen_uarray_asc(p0, ctx),
            cgen_uarray_field(p0, 0, ctx),
            cgen_uarray_field(p0, 1, ctx),
            "low");
      }
      else if (icmp(builtin, "uarray_high")) {
         return LLVMBuildSelect(
            builder, cgen_uarray_asc(p0, ctx),
            cgen_uarray_field(p0, 1, ctx),
            cgen_uarray_field(p0, 0, ctx),
            "high");
      }
   }

   const int nparams = tree_params(t);
   LLVMValueRef args[nparams];
   type_t arg_types[nparams];
   cgen_call_args(t, args, arg_types, ctx);

   // Regular builtin functions
   if (builtin) {
      assert(nparams > 0);
      const bool real = cgen_is_real(arg_types[0]);

      if (icmp(builtin, "mul")) {
         if (real)
            return LLVMBuildFMul(builder, args[0], args[1], "");
         else {
            cgen_promote_arith(tree_type(t), args, nparams);
            return LLVMBuildMul(builder, args[0], args[1], "");
         }
      }
      else if (icmp(builtin, "add")) {
         if (real)
            return LLVMBuildFAdd(builder, args[0], args[1], "");
         else {
            cgen_promote_arith(tree_type(t), args, nparams);
            return LLVMBuildAdd(builder, args[0], args[1], "");
         }
      }
      else if (icmp(builtin, "sub")) {
         if (real)
            return LLVMBuildFSub(builder, args[0], args[1], "");
         else {
            cgen_promote_arith(tree_type(t), args, nparams);
            return LLVMBuildSub(builder, args[0], args[1], "");
         }
      }
      else if (icmp(builtin, "div")) {
         if (real)
            return LLVMBuildFDiv(builder, args[0], args[1], "");
         else {
            cgen_promote_arith(tree_type(t), args, nparams);
            return LLVMBuildSDiv(builder, args[0], args[1], "");
         }
      }
      else if (icmp(builtin, "eq")) {
         if (real)
            return LLVMBuildFCmp(builder, LLVMRealUEQ, args[0], args[1], "");
         else
            return LLVMBuildICmp(builder, LLVMIntEQ, args[0], args[1], "");
      }
      else if (icmp(builtin, "neq")) {
         if (real)
            return LLVMBuildFCmp(builder, LLVMRealUNE, args[0], args[1], "");
         else
            return LLVMBuildICmp(builder, LLVMIntNE, args[0], args[1], "");
      }
      else if (icmp(builtin, "lt")) {
         if (real)
            return LLVMBuildFCmp(builder, LLVMRealULT, args[0], args[1], "");
         else
            return LLVMBuildICmp(builder, LLVMIntSLT, args[0], args[1], "");
      }
      else if (icmp(builtin, "gt")) {
         if (real)
            return LLVMBuildFCmp(builder, LLVMRealUGT, args[0], args[1], "");
         else
            return LLVMBuildICmp(builder, LLVMIntSGT, args[0], args[1], "");
      }
      else if (icmp(builtin, "leq")) {
         if (real)
            return LLVMBuildFCmp(builder, LLVMRealULE, args[0], args[1], "");
         else
            return LLVMBuildICmp(builder, LLVMIntSLE, args[0], args[1], "");
      }
      else if (icmp(builtin, "geq")) {
         if (real)
            return LLVMBuildFCmp(builder, LLVMRealUGE, args[0], args[1], "");
         else
            return LLVMBuildICmp(builder, LLVMIntSGE, args[0], args[1], "");
      }
      else if (icmp(builtin, "neg")) {
         if (real)
            return LLVMBuildFNeg(builder, args[0], "neg");
         else
            return LLVMBuildNeg(builder, args[0], "neg");
      }
      else if (icmp(builtin, "not"))
         return LLVMBuildNot(builder, args[0], "");
      else if (icmp(builtin, "and"))
         return LLVMBuildAnd(builder, args[0], args[1], "");
      else if (icmp(builtin, "or"))
         return LLVMBuildOr(builder, args[0], args[1], "");
      else if (icmp(builtin, "xor"))
         return LLVMBuildXor(builder, args[0], args[1], "");
      else if (icmp(builtin, "xnor"))
         return LLVMBuildNot(builder,
                             LLVMBuildXor(builder, args[0], args[1], ""), "");
      else if (icmp(builtin, "mod"))
         return LLVMBuildURem(builder, args[0], args[1], "");
      else if (icmp(builtin, "rem"))
         return LLVMBuildSRem(builder, args[0], args[1], "");
      else if (icmp(builtin, "exp")) {
         if (real) {
            LLVMValueRef cast[] = {
               args[0],
               LLVMBuildSIToFP(builder, args[1], LLVMDoubleType(), ""),
            };
            return LLVMBuildCall(builder, llvm_fn("llvm.pow.f64"), cast, 2, "");
         }
         else {
            LLVMValueRef cast[] = {
               LLVMBuildUIToFP(builder, args[0], LLVMDoubleType(), ""),
               LLVMBuildUIToFP(builder, args[1], LLVMDoubleType(), "")
            };
            return LLVMBuildFPToUI(
               builder,
               LLVMBuildCall(builder, llvm_fn("llvm.pow.f64"), cast, 2, ""),
               llvm_type(tree_type(t)),
               "pow");
         }
      }
      else if (icmp(builtin, "abs")) {
         LLVMValueRef cmp = real
            ? LLVMBuildFCmp(builder, LLVMRealULT, args[0], llvm_real(0.0), "")
            : LLVMBuildICmp(builder, LLVMIntSLT, args[0], llvm_int32(0), "");
         return LLVMBuildSelect(
            builder, cmp,
            (real ? LLVMBuildFNeg : LLVMBuildNeg)(builder, args[0], ""),
            args[0], "abs");
      }
      else if (icmp(builtin, "aeq"))
         return cgen_array_rel(args[0], args[1], arg_types[0], arg_types[1],
                               LLVMIntEQ, ctx);
      else if (icmp(builtin, "aneq"))
         return cgen_array_rel(args[0], args[1], arg_types[0], arg_types[1],
                               LLVMIntNE, ctx);
      else if (icmp(builtin, "req"))
         return cgen_record_eq(args[0], args[1], arg_types[0], arg_types[1],
                               LLVMIntEQ, ctx);
      else if (icmp(builtin, "rneq"))
         return cgen_record_eq(args[0], args[1], arg_types[0], arg_types[1],
                               LLVMIntNE, ctx);
      else if (icmp(builtin, "image")) {
         const bool is_signed =
            (type_kind(type_base_recur(arg_types[0])) == T_INTEGER);
         LLVMOpcode op = real ? LLVMBitCast : (is_signed ? LLVMSExt : LLVMZExt);
         LLVMValueRef res = LLVMBuildAlloca(builder,
                                            llvm_uarray_type(LLVMInt8Type(), 1),
                                            "image");
         LLVMValueRef iargs[] = {
            LLVMBuildCast(builder, op, args[0], LLVMInt64Type(), ""),
            llvm_int32(tree_index(tree_param(t, 0).value)),
            LLVMBuildPointerCast(builder, mod_name,
                                 LLVMPointerType(LLVMInt8Type(), 0), ""),
            res
         };
         LLVMBuildCall(builder, llvm_fn("_image"), iargs, ARRAY_LEN(iargs), "");
         return LLVMBuildLoad(builder, res, "");
      }
      else if (icmp(builtin, "succ")) {
         return LLVMBuildAdd(builder, args[0],
                             LLVMConstInt(llvm_type(arg_types[0]), 1, false),
                             "succ");
      }
      else if (icmp(builtin, "pred")) {
         return LLVMBuildAdd(builder, args[0],
                             LLVMConstInt(llvm_type(arg_types[0]), -1, false),
                             "pred");
      }
      else if (icmp(builtin, "leftof")) {
         range_t r = type_dim(tree_type(t), 0);
         int dir = (r.kind == RANGE_TO ? -1 : 1);
         return LLVMBuildAdd(builder, args[0],
                             LLVMConstInt(llvm_type(arg_types[0]), dir, false),
                             "leftof");
      }
      else if (icmp(builtin, "rightof")) {
         range_t r = type_dim(tree_type(t), 0);
         int dir = (r.kind == RANGE_TO ? 1 : -1);
         return LLVMBuildAdd(builder, args[0],
                             LLVMConstInt(llvm_type(arg_types[0]), dir, false),
                             "rightof");
      }
      else if (icmp(builtin, "length")) {
         assert(!cgen_const_bounds(arg_types[1]));
         return cgen_array_len(arg_types[1],
                               assume_int(tree_param(t, 0).value) - 1,
                               args[1]);
      }
      else if (icmp(builtin, "uarray_dircmp")) {
         LLVMValueRef dir_eq = LLVMBuildICmp(
            builder, LLVMIntEQ,
            cgen_array_dir(arg_types[0], args[0]),
            LLVMBuildIntCast(builder, args[1], LLVMInt8Type(), ""),
            "diff_eq");
         LLVMValueRef neg = LLVMBuildNeg(builder, args[2], "neg");
         return LLVMBuildSelect(builder, dir_eq, args[2], neg, "dirmul");
      }
      else if (icmp(builtin, "alt"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_param(t, 1).value),
                               LLVMIntSLT, ctx);
      else if (icmp(builtin, "agt"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_param(t, 1).value),
                               LLVMIntSGT, ctx);
      else if (icmp(builtin, "aleq"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_param(t, 1).value),
                               LLVMIntSLE, ctx);
      else if (icmp(builtin, "ageq"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_param(t, 1).value),
                               LLVMIntSGE, ctx);
      else if (icmp(builtin, "endfile"))
         return LLVMBuildCall(builder, llvm_fn("_endfile"), args, 1, "");
      else
         fatal("cannot generate code for builtin %s", istr(builtin));
   }
   else {
      return LLVMBuildCall(builder, cgen_fdecl(decl), args, nparams, "");
   }
}

static LLVMValueRef cgen_scalar_signal_ref(tree_t decl, cgen_ctx_t *ctx)
{
   LLVMValueRef signal = tree_attr_ptr(decl, sig_struct_i);
   LLVMValueRef ptr =
      LLVMBuildStructGEP(builder, signal, SIGNAL_RESOLVED, "");
   LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
   return LLVMBuildIntCast(builder, deref,
                           llvm_type(tree_type(decl)), "");
}

static LLVMValueRef cgen_ref(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   bool needs_load = false;

   tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_ENUM_LIT:
      return LLVMConstInt(llvm_type(tree_type(t)), tree_pos(decl), false);

   case T_CONST_DECL:
   case T_UNIT_DECL:
   case T_VAR_DECL:
   case T_FILE_DECL:
      {
         LLVMValueRef ptr = cgen_get_var(decl, ctx);
         if (type_is_array(tree_type(decl)))
            return ptr;
         else
            return LLVMBuildLoad(builder, ptr, "");
      }

   case T_PORT_DECL:
      needs_load = ((tree_port_mode(decl) == PORT_INOUT)
                    && !type_is_array(tree_type(decl)));
      // Fall-through

   case T_SIGNAL_DECL:
      if (cgen_get_class(decl) == C_SIGNAL) {
         type_t type = tree_type(decl);
         if (type_is_array(type))
            return cgen_vec_load(cgen_sig_struct(decl), type, type, false, ctx);
         else
            return cgen_scalar_signal_ref(decl, ctx);
      }
      else {
         LLVMValueRef var = cgen_get_var(decl, ctx);
         return needs_load ? LLVMBuildLoad(builder, var, "") : var;
      }

   case T_TYPE_DECL:
      // The value of a type reference should never be used
      return NULL;

   default:
      fatal("cannot generate code for %s", tree_kind_str(kind));
   }
}

static LLVMValueRef cgen_array_data_ptr(type_t type, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      // Unwrap array to get data pointer
      return LLVMBuildExtractValue(builder, var, 0, "aptr");
   }
   else {
      LLVMValueRef indexes[] = { llvm_int32(0) };
      return LLVMBuildPointerCast(
         builder,
         LLVMBuildGEP(builder, var,
                      indexes, ARRAY_LEN(indexes), ""),
         LLVMPointerType(llvm_type(type_elem(type)), 0), "aptr");
   }
}

static LLVMValueRef cgen_array_ref(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = NULL;
   class_t class = C_VARIABLE;
   LLVMValueRef array = NULL;

   tree_t value = tree_value(t);
   if (tree_kind(value) == T_REF) {
      decl = tree_ref(value);

      class = cgen_get_class(decl);

      switch (class) {
      case C_VARIABLE:
      case C_CONSTANT:
      case C_DEFAULT:
         array = cgen_get_var(decl, ctx);
         break;
      case C_SIGNAL:
         array = tree_attr_ptr(decl, sig_struct_i);
         break;
      default:
         assert(false);
      }
   }
   else
      array = cgen_expr(value, ctx);

   type_t type = tree_type(value);

   LLVMValueRef idx = llvm_int32(0);
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      LLVMValueRef offset = cgen_expr(p.value, ctx);

      if (i > 0) {
         range_t r = type_dim(type, i - 1);
         int64_t low, high;
         range_bounds(r, &low, &high);

         bounds_kind_t kind =
            (r.kind == RANGE_TO) ? BOUNDS_ARRAY_TO : BOUNDS_ARRAY_DOWNTO;
         cgen_check_bounds(p.value, llvm_int32(kind), offset,
                           llvm_int32(low), llvm_int32(high), ctx);

         LLVMValueRef stride = llvm_int32(high - low + 1);
         idx = LLVMBuildMul(builder, idx, stride, "");
      }
      else
         cgen_check_array_bounds(p.value, type, array, offset, ctx);

      idx = LLVMBuildAdd(builder, idx,
                         cgen_array_off(offset, array, type, ctx, 0), "idx");
   }

   switch (class) {
   case C_VARIABLE:
   case C_CONSTANT:
   case C_DEFAULT:
      {
         LLVMValueRef data = cgen_array_data_ptr(type, array);
         LLVMValueRef ptr = LLVMBuildGEP(builder, data, &idx, 1, "");
         if (type_is_array(tree_type(t)))
            return ptr;
         else
            return LLVMBuildLoad(builder, ptr, "");
      }

   case C_SIGNAL:
      {
         assert(decl != NULL);

         LLVMValueRef signal;
         if (type_kind(type) == T_UARRAY) {
            // Unwrap array to signal array
            array = LLVMBuildExtractValue(builder, array, 0, "aptr");

            LLVMValueRef indexes[] = { idx };
            signal = LLVMBuildGEP(builder, array,
                                  indexes, ARRAY_LEN(indexes), "");
         }
         else {
            LLVMValueRef indexes[] = { llvm_int32(0), idx };
            signal = LLVMBuildGEP(builder, array,
                                  indexes, ARRAY_LEN(indexes), "");
         }

         type_t elem_type = type_elem(type);
         if (type_is_array(elem_type)) {
            // Load this sub-array into a temporary variable
            return cgen_vec_load(signal, elem_type, elem_type, false, ctx);
         }
         else {
            LLVMValueRef ptr =
               LLVMBuildStructGEP(builder, signal, SIGNAL_RESOLVED, "");
            LLVMValueRef deref = LLVMBuildLoad(builder, ptr, "");
            return LLVMBuildIntCast(builder, deref,
                                    llvm_type(tree_type(t)), "");
         }
      }

   default:
      assert(false);
   }
}

static LLVMValueRef cgen_array_slice(tree_t t, cgen_ctx_t *ctx)
{
   assert(tree_kind(tree_value(t)) == T_REF);

   tree_t decl = tree_ref(tree_value(t));
   type_t type = tree_type(decl);

   switch (cgen_get_class(decl)) {
   case C_VARIABLE:
   case C_DEFAULT:
   case C_CONSTANT:
      {
         LLVMValueRef array = cgen_get_var(decl, ctx);
         return cgen_get_slice(array, type, tree_range(t), ctx);
      }

   case C_SIGNAL:
      return cgen_vec_load(cgen_sig_struct(decl), type, tree_type(t),
                           false, ctx);

   default:
      assert(false);
   }
}

static void cgen_copy_vals(LLVMValueRef *dst, LLVMValueRef *src,
                           unsigned n, bool backwards)
{
   while (n--) {
      *dst = *src;
      ++src;
      dst += (backwards ? -1 : 1);
   }
}

static LLVMValueRef *cgen_const_aggregate(tree_t t, cgen_ctx_t *ctx,
                                          unsigned dim, unsigned *n_elems)
{
   type_t type = tree_type(t);

   *n_elems = 1;
   const int ndims = type_dims(type);
   for (int i = dim; i < ndims; i++) {
      range_t r = type_dim(type, i);

      int64_t low, high;
      range_bounds(r, &low, &high);

      if (high < low)
         *n_elems = 0;
      else
         *n_elems *= (high - low + 1);
   }

   if (*n_elems == 0)
      return NULL;

   LLVMValueRef *vals = xmalloc(*n_elems * sizeof(LLVMValueRef));

   for (unsigned i = 0; i < *n_elems; i++)
      vals[i] = NULL;

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      assoc_t a = tree_assoc(t, i);

      LLVMValueRef *sub;
      unsigned nsub;
      if (dim < type_dims(type) - 1)
         sub = cgen_const_aggregate(a.value, ctx, 0 /* XXX */, &nsub);
      else if (tree_kind(a.value) == T_AGGREGATE) {
         sub  = xmalloc(sizeof(LLVMValueRef));
         nsub = 1;

         type_t sub_type = tree_type(a.value);
         if (type_is_array(sub_type)) {
            unsigned nvals;
            LLVMValueRef *v = cgen_const_aggregate(a.value, ctx, 0, &nvals);
            LLVMTypeRef ltype = llvm_type(type_elem(tree_type(a.value)));

            *sub = LLVMConstArray(ltype, v, nvals);
         }
         else if (type_kind(sub_type) == T_RECORD) {
            *sub = cgen_const_record(a.value, ctx);
         }
         else
            assert(false);
      }
      else {
         sub  = xmalloc(sizeof(LLVMValueRef));
         *sub = cgen_expr(a.value, ctx);
         nsub = 1;
      }

      range_t r = type_dim(type, dim);

      int64_t low, high;
      range_bounds(r, &low, &high);

      switch (a.kind) {
      case A_POS:
         if (r.kind == RANGE_TO)
            cgen_copy_vals(vals + (i * nsub), sub, nsub, false);
         else
            cgen_copy_vals(vals + ((*n_elems - i - 1) * nsub),
                           sub, nsub, true);
         break;

      case A_NAMED:
         cgen_copy_vals(vals + ((assume_int(a.name) - low) * nsub),
                        sub, nsub, false);
         break;

      case A_OTHERS:
         assert((*n_elems % nsub) == 0);
         for (unsigned j = 0; j < (*n_elems / nsub); j++) {
            if (vals[j * nsub] == NULL)
               cgen_copy_vals(vals + (j * nsub), sub, nsub, false);
         }
         break;

      case A_RANGE:
         {
            int64_t r_low, r_high;
            range_bounds(a.range, &r_low, &r_high);

            for (int j = r_low; j <= r_high; j++)
               cgen_copy_vals(vals + ((j - low) * nsub), sub, nsub, false);
         }
         break;
      }

      free(sub);
   }

   for (unsigned i = 0; i < *n_elems; i++)
      assert(vals[i] != NULL);

   return vals;
}

static LLVMValueRef cgen_dyn_aggregate(tree_t t, cgen_ctx_t *ctx)
{
   // Generate code to fill in the aggregate at run time

   type_t type = tree_type(t);

   LLVMBasicBlockRef test_bb  = LLVMAppendBasicBlock(ctx->fn, "da_test");
   LLVMBasicBlockRef body_bb  = LLVMAppendBasicBlock(ctx->fn, "da_body");
   LLVMBasicBlockRef exit_bb  = LLVMAppendBasicBlock(ctx->fn, "da_exit");

   // Prelude
   LLVMValueRef a = cgen_tmp_var(type, "dyn_tmp", ctx);
   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, llvm_int32(0), i);

   LLVMValueRef data = cgen_array_data_ptr(type, a);
   LLVMValueRef len = cgen_array_len(type, -1, a);

   LLVMValueRef def = NULL;
   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      assoc_t a = tree_assoc(t, i);
      if (a.kind == A_OTHERS) {
         def = cgen_expr(a.value, ctx);
         break;
      }
   }

   LLVMBuildBr(builder, test_bb);

   if (def == NULL)
      def = LLVMGetUndef(llvm_type(type_elem(type)));

   // Loop test
   LLVMPositionBuilderAtEnd(builder, test_bb);
   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i");
   LLVMValueRef ge = LLVMBuildICmp(builder, LLVMIntUGE, i_loaded,
                                   len, "ge");
   LLVMBuildCondBr(builder, ge, exit_bb, body_bb);

   // Loop body
   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef what = def;
   for (int i = 0; i < nassocs; i++) {
      assoc_t a = tree_assoc(t, i);

      switch (a.kind) {
      case A_POS:
         {
            LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ, i_loaded,
                                            llvm_int32(a.pos), "");
            what = LLVMBuildSelect(builder, eq, cgen_expr(a.value, ctx),
                                   what, "");
         }
         break;

      case A_NAMED:
         {
            LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ, i_loaded,
                                            cgen_expr(a.name, ctx), "");
            what = LLVMBuildSelect(builder, eq, cgen_expr(a.value, ctx),
                                   what, "");
         }
         break;

      case A_RANGE:
         {
            LLVMIntPredicate lpred =
               (a.range.kind == RANGE_TO ? LLVMIntSGE : LLVMIntSLE);
            LLVMIntPredicate rpred =
               (a.range.kind == RANGE_TO ? LLVMIntSLE : LLVMIntSGE);

            LLVMValueRef lcmp =
               LLVMBuildICmp(builder, lpred, i_loaded,
                             cgen_expr(a.range.left, ctx), "lcmp");
            LLVMValueRef rcmp =
               LLVMBuildICmp(builder, rpred, i_loaded,
                             cgen_expr(a.range.right, ctx), "rcmp");
            LLVMValueRef in = LLVMBuildOr(builder, lcmp, rcmp, "in");

            what = LLVMBuildSelect(builder, in, cgen_expr(a.value, ctx),
                                   what, "");
         }
         break;

      case A_OTHERS:
         break;
      }
   }

   LLVMValueRef indexes[] = { i_loaded };
   LLVMValueRef ptr = LLVMBuildGEP(builder, data, indexes,
                                   ARRAY_LEN(indexes), "ptr");
   LLVMBuildStore(builder, what, ptr);

   LLVMValueRef inc =
      LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "inc");
   LLVMBuildStore(builder, inc, i);
   LLVMBuildBr(builder, test_bb);

   // Epilogue
   LLVMPositionBuilderAtEnd(builder, exit_bb);

   return a;
}

static int cgen_field_index(type_t type, ident_t field)
{
   // Lookup the position of this field in the record type

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      if (tree_ident(type_field(type, i)) == field)
         return i;
   }
   assert(false);
}

static LLVMValueRef cgen_const_record(tree_t t, cgen_ctx_t *ctx)
{
   type_t type = tree_type(t);
   const int nfields = type_fields(type);
   const int nassocs = tree_assocs(t);

   LLVMValueRef vals[nfields];
   for (int i = 0; i < nfields; i++)
      vals[i] = NULL;

   for (int i = 0; i < nassocs; i++) {
      assoc_t a = tree_assoc(t, i);

      type_t value_type = tree_type(a.value);
      LLVMValueRef v;
      if (type_is_array(value_type)) {
         unsigned nvals;
         LLVMValueRef *vals = cgen_const_aggregate(a.value, ctx, 0, &nvals);
         LLVMTypeRef ltype = llvm_type(type_elem(value_type));
         v = LLVMConstArray(ltype, vals, nvals);
      }
      else
         v = cgen_expr(a.value, ctx);

      switch (a.kind) {
      case A_POS:
         vals[a.pos] = v;
         break;

      case A_NAMED:
         {
            int index = cgen_field_index(type, tree_ident(a.name));
            vals[index] = v;
         }
         break;

      case A_OTHERS:
         for (int j = 0; j < nfields; j++) {
            if (vals[j] == NULL)
               vals[j] = v;
         }
         break;

      case A_RANGE:
         assert(false);
      }
   }

   for (int i = 0; i < nfields; i++)
      assert(vals[i] != NULL);

   return LLVMConstNamedStruct(llvm_type(type), vals, nfields);
}

static LLVMValueRef cgen_aggregate(tree_t t, cgen_ctx_t *ctx)
{
   type_t type = tree_type(t);

   if (type_is_array(type)) {
      if (cgen_const_bounds(type) && cgen_is_const(t)) {
         unsigned nvals;
         LLVMValueRef *vals = cgen_const_aggregate(t, ctx, 0, &nvals);

         LLVMTypeRef ltype = llvm_type(type_elem(type));

         LLVMTypeRef at = LLVMArrayType(ltype, nvals);
         LLVMValueRef g = LLVMAddGlobal(module, at, "");
         LLVMSetGlobalConstant(g, true);
         LLVMSetLinkage(g, LLVMInternalLinkage);
         LLVMSetInitializer(g, LLVMConstArray(ltype, vals, nvals));

         free(vals);
         return g;
      }
      else
         return cgen_dyn_aggregate(t, ctx);
   }
   else if (type_kind(type) == T_RECORD) {
      if (cgen_is_const(t))
         return cgen_const_record(t, ctx);
      else
         fatal_at(tree_loc(t), "sorry, non-constant record aggregates "
                  "are not supported yet");
   }
   else
      assert(false);
}

static LLVMValueRef cgen_concat(tree_t t, cgen_ctx_t *ctx)
{
   type_t type = tree_type(t);
   LLVMValueRef var = cgen_tmp_var(tree_type(t), "", ctx);

   assert(tree_params(t) == 2);
   tree_t args[] = {
      tree_param(t, 0).value,
      tree_param(t, 1).value
   };

   LLVMValueRef args_ll[] = {
      cgen_expr(args[0], ctx),
      cgen_expr(args[1], ctx)
   };

   LLVMValueRef off = NULL;

   if (type_is_array(tree_type(args[0]))) {
      cgen_array_copy(tree_type(args[0]), type, args_ll[0], var, NULL, ctx);
      off = cgen_array_len(tree_type(args[0]), 0, args_ll[0]);
   }
   else {
      LLVMValueRef zero = llvm_int32(0);
      LLVMValueRef data = cgen_array_data_ptr(type, var);
      LLVMValueRef ptr = LLVMBuildGEP(builder, data, &zero, 1, "");
      LLVMBuildStore(builder, args_ll[0], ptr);

      off = llvm_int32(1);
   }

   if (type_is_array(tree_type(args[1])))
      cgen_array_copy(tree_type(args[1]), type, args_ll[1], var, off, ctx);
   else {
      LLVMValueRef data = cgen_array_data_ptr(type, var);
      LLVMValueRef ptr = LLVMBuildGEP(builder, data, &off, 1, "");
      LLVMBuildStore(builder, args_ll[1], ptr);
   }

   return var;
}

static LLVMValueRef cgen_type_conv(tree_t t, cgen_ctx_t *ctx)
{
   tree_t value = tree_param(t, 0).value;

   type_t from = type_base_recur(tree_type(value));
   type_t to   = type_base_recur(tree_type(t));

   type_kind_t from_k = type_kind(from);
   type_kind_t to_k   = type_kind(to);

   LLVMValueRef value_ll = cgen_expr(value, ctx);

   if ((from_k == T_REAL) && (to_k == T_INTEGER))
      return LLVMBuildFPToSI(builder, value_ll, llvm_type(to), "");
   else if ((from_k == T_INTEGER) && (to_k == T_REAL))
      return LLVMBuildSIToFP(builder, value_ll, llvm_type(to), "");
   else if (!cgen_const_bounds(to)) {
      // Need to wrap in metadata
      return cgen_array_meta_1(
         to,
         cgen_array_left(from, value_ll),
         cgen_array_right(from, value_ll),
         cgen_array_dir(from, value_ll),
         cgen_array_data_ptr(from, value_ll));
   }
   else {
      // No conversion to perform
      return value_ll;
   }
}

static LLVMValueRef cgen_record_ref(tree_t t, cgen_ctx_t *ctx)
{
   tree_t value = tree_value(t);
   int index    = cgen_field_index(tree_type(value), tree_ident(t));

   if (type_is_array(tree_type(t))) {
      LLVMValueRef rec = cgen_var_lvalue(value, ctx);
      return LLVMBuildStructGEP(builder, rec, index, "field");
   }
   else {
      LLVMValueRef rec = cgen_expr(tree_value(t), ctx);
      return LLVMBuildExtractValue(builder, rec, index, "field");
   }
}

static LLVMValueRef cgen_new(tree_t t, cgen_ctx_t *ctx)
{
   type_t type = type_access(tree_type(t));

   LLVMValueRef ptr = LLVMBuildMalloc(builder, llvm_type(type), "");

   if (type_is_array(type) && !cgen_const_bounds(type)) {
      // Need to allocate memory for both the array and its metadata

      range_t r = tree_range(tree_value(t));

      LLVMTypeRef elem = llvm_type(type_elem(type));

      LLVMValueRef meta =
         cgen_array_meta_1(type,
                           cgen_expr(r.left, ctx),
                           cgen_expr(r.right, ctx),
                           llvm_int8(r.kind),
                           LLVMConstNull(LLVMPointerType(elem, 0)));

      LLVMValueRef len  = cgen_array_len(type, -1, meta);
      LLVMValueRef data = LLVMBuildArrayMalloc(builder, elem, len, "data");

      meta = LLVMBuildInsertValue(builder, meta, data, 0, "");

      LLVMBuildStore(builder, meta, ptr);
   }

   return ptr;
}

static LLVMValueRef cgen_all(tree_t t, cgen_ctx_t *ctx)
{
   type_t type = tree_type(t);
   LLVMValueRef ptr = cgen_expr(tree_value(t), ctx);
   if (type_is_array(type) && cgen_const_bounds(type))
      return ptr;
   else
      return LLVMBuildLoad(builder, ptr, "all");
}

static LLVMValueRef cgen_expr(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return cgen_literal(t);
   case T_FCALL:
      return cgen_fcall(t, ctx);
   case T_REF:
      return cgen_ref(t, ctx);
   case T_ARRAY_REF:
      return cgen_array_ref(t, ctx);
   case T_ARRAY_SLICE:
      return cgen_array_slice(t, ctx);
   case T_AGGREGATE:
      return cgen_aggregate(t, ctx);
   case T_CONCAT:
      return cgen_concat(t, ctx);
   case T_TYPE_CONV:
      return cgen_type_conv(t, ctx);
   case T_RECORD_REF:
      return cgen_record_ref(t, ctx);
   case T_NEW:
      return cgen_new(t, ctx);
   case T_ALL:
      return cgen_all(t, ctx);
   default:
      fatal("missing cgen_expr for %s", tree_kind_str(tree_kind(t)));
   }
}

static void cgen_sched_process(LLVMValueRef after)
{
   LLVMValueRef args[] = { after };
   LLVMBuildCall(builder, llvm_fn("_sched_process"), args, 1, "");
}

static void cgen_sched_event(tree_t on)
{
   if (tree_kind(on) != T_REF) {
      // It is possible for constant folding to replace a signal with
      // a constant which will then appear in a sensitivity list so
      // just ignore it
      return;
   }

   tree_t decl = tree_ref(on);

   tree_kind_t kind = tree_kind(decl);
   if ((kind != T_SIGNAL_DECL) && (kind != T_PORT_DECL)) {
      // As above, a port could have been rewritten to reference a
      // constant declaration or enumeration literal, in which case
      // just ignore it too
      return;
   }

   type_t type = tree_type(decl);

   int32_t n = 1;
   if (type_kind(type) == T_CARRAY) {
      int64_t low, high;
      range_bounds(type_dim(type, 0), &low, &high);
      n = high - low + 1;
   }

   LLVMValueRef signal = tree_attr_ptr(decl, sig_struct_i);
   LLVMValueRef args[] = {
      llvm_void_cast(signal),
      llvm_int32(n)
   };
   LLVMBuildCall(builder, llvm_fn("_sched_event"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_wait(tree_t t, cgen_ctx_t *ctx)
{
   if (tree_has_delay(t))
      cgen_sched_process(cgen_expr(tree_delay(t), ctx));

   for (unsigned i = 0; i < tree_triggers(t); i++)
      cgen_sched_event(tree_trigger(t, i));

   // Find the basic block to jump to when the process is next scheduled
   struct proc_entry *it;
   for (it = ctx->entry_list; it && it->wait != t; it = it->next)
      ;
   assert(it != NULL);

   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMBuildStore(builder, llvm_int32(it->state_num), state_ptr);

   if (ctx->proc == NULL) {
      // We are inside a procedure so return a pointer to the dynamic context
      LLVMBuildRet(builder, llvm_void_cast(ctx->state));
   }
   else
      LLVMBuildRetVoid(builder);

   LLVMPositionBuilderAtEnd(builder, it->bb);
}

static LLVMValueRef cgen_var_lvalue(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_REF:
      return cgen_get_var(tree_ref(t), ctx);

   case T_ARRAY_REF:
      {
         type_t type = tree_type(tree_value(t));

         param_t p = tree_param(t, 0);
         assert(p.kind == P_POS);

         LLVMValueRef var = cgen_var_lvalue(tree_value(t), ctx);
         LLVMValueRef idx = cgen_expr(p.value, ctx);

         cgen_check_array_bounds(p.value, type, var, idx, ctx);

         LLVMValueRef off = cgen_array_off(idx, var, type, ctx, 0);
         LLVMValueRef data = cgen_array_data_ptr(type, var);
         return LLVMBuildGEP(builder, data, &off, 1, "");
      }

   case T_ARRAY_SLICE:
      {
         LLVMValueRef array = cgen_var_lvalue(tree_value(t), ctx);

         type_t ty = tree_type(tree_value(t));
         return cgen_get_slice(array, ty, tree_range(t), ctx);
      }

   case T_RECORD_REF:
      {
         tree_t value = tree_value(t);
         int index    = cgen_field_index(tree_type(value), tree_ident(t));

         LLVMValueRef rec = cgen_var_lvalue(value, ctx);
         return LLVMBuildStructGEP(builder, rec, index, "");
      }

   case T_ALL:
      {
         LLVMValueRef ptr = cgen_expr(tree_value(t), ctx);

         type_t type = tree_type(t);
         if (type_is_array(type) && !cgen_const_bounds(type))
            return LLVMBuildLoad(builder, ptr, "all");
         else {
            LLVMValueRef indexes[] = {
               llvm_int32(0)
            };
            return LLVMBuildGEP(builder, ptr, indexes,
                                ARRAY_LEN(indexes), "all");
         }
      }

   default:
      fatal("missing cgen_var_lvalue for %s", tree_kind_str(tree_kind(t)));
   }
}

static void cgen_var_assign(tree_t t, cgen_ctx_t *ctx)
{
   LLVMValueRef rhs = cgen_expr(tree_value(t), ctx);
   type_t value_type = tree_type(tree_value(t));

   tree_t target = tree_target(t);

   LLVMValueRef lhs = cgen_var_lvalue(target, ctx);

   type_t ty = tree_type(target);
   if (type_is_array(ty))
      cgen_array_copy(value_type, ty, rhs, lhs, NULL, ctx);
   else
      LLVMBuildStore(builder, rhs, lhs);
}

static void cgen_sched_waveform(LLVMValueRef signal, LLVMValueRef value,
                                LLVMValueRef after, LLVMValueRef reject,
                                cgen_ctx_t *ctx)
{
   LLVMValueRef args[] = {
      llvm_void_cast(signal),
      LLVMBuildZExt(builder, value, LLVMInt64Type(), ""),
      after,
      reject
   };
   LLVMBuildCall(builder, llvm_fn("_sched_waveform"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_sched_waveform_vec(LLVMValueRef lhs, type_t lhs_type,
                                    LLVMValueRef rhs, type_t rhs_type,
                                    LLVMValueRef after, LLVMValueRef reject,
                                    cgen_ctx_t *ctx)
{
   assert(type_is_array(lhs_type));

   LLVMValueRef rhs_data = cgen_array_data_ptr(rhs_type, rhs);
   LLVMValueRef n_elems = cgen_array_len_recur(rhs_type, rhs);

   LLVMValueRef ldir = cgen_array_dir(lhs_type, lhs);
   LLVMValueRef rdir = cgen_array_dir(rhs_type, rhs);

   LLVMValueRef reverse = LLVMBuildICmp(builder, LLVMIntNE,
                                        ldir, rdir, "reverse");

   LLVMValueRef args[] = {
      llvm_void_cast(lhs),
      llvm_void_cast(rhs_data),
      n_elems,
      cgen_array_elem_size(rhs_type),
      after,
      reject,
      reverse
   };
   LLVMBuildCall(builder, llvm_fn("_sched_waveform_vec"),
                 args, ARRAY_LEN(args), "");
}

static LLVMValueRef cgen_signal_lvalue(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_REF:
      return cgen_array_signal_ptr(tree_ref(t), llvm_int32(0));

   case T_ARRAY_REF:
      {
         param_t p = tree_param(t, 0);
         assert(p.kind == P_POS);

         type_t type = tree_type(tree_value(t));

         LLVMValueRef idx = cgen_expr(p.value, ctx);

         if (tree_kind(tree_value(t)) == T_REF) {
            if (type_kind(type) == T_UARRAY) {
               tree_t decl = tree_ref(tree_value(t));
               assert(type_is_array(tree_type(decl)));

               LLVMValueRef meta = tree_attr_ptr(decl, sig_struct_i);
               assert(meta != NULL);

               cgen_check_array_bounds(p.value, type, meta, idx, ctx);

               LLVMValueRef sig_array = cgen_array_data_ptr(type, meta);
               LLVMValueRef off = cgen_array_off(idx, meta, type, ctx, 0);

               LLVMValueRef indexes[] = { off };
               return LLVMBuildGEP(builder, sig_array,
                                   indexes, ARRAY_LEN(indexes), "");
            }
            else {
               LLVMValueRef off = cgen_array_off(idx, NULL, type, ctx, 0);

               tree_t decl = tree_ref(tree_value(t));
               assert(type_is_array(tree_type(decl)));

               cgen_check_array_bounds(p.value, type, NULL, idx, ctx);

               LLVMValueRef signal = cgen_array_signal_ptr(decl, off);
               if (type_is_array(type_elem(type))) {
                  LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
                  return LLVMBuildGEP(builder, signal,
                                      indexes, ARRAY_LEN(indexes), "");
               }
               else
                  return signal;
            }
         }
         else {
            assert(type_kind(type) != T_UARRAY);

            cgen_check_array_bounds(p.value, type, NULL, idx, ctx);

            LLVMValueRef off = cgen_array_off(idx, NULL, type, ctx, 0);
            LLVMValueRef p_base = cgen_signal_lvalue(tree_value(t), ctx);

            LLVMValueRef indexes[] = { off };
            return LLVMBuildGEP(builder, p_base,
                                indexes, ARRAY_LEN(indexes), "");
         }
      }
      break;

   case T_ARRAY_SLICE:
      {
         assert(tree_kind(tree_value(t)) == T_REF);

         tree_t decl = tree_ref(tree_value(t));
         assert(type_kind(tree_type(decl)) == T_CARRAY);

         range_t r = tree_range(t);

         LLVMValueRef left  = cgen_expr(r.left, ctx);
         LLVMValueRef right = cgen_expr(r.right, ctx);

         type_t val_type = tree_type(tree_value(t));

         cgen_check_array_bounds(r.left, val_type, NULL, left, ctx);
         cgen_check_array_bounds(r.right, val_type, NULL, right, ctx);

         LLVMValueRef low = (r.kind == RANGE_TO ? left : right);
         return cgen_array_signal_ptr(decl, low);
      }
      break;

   default:
      assert(false);
   }
}

static void cgen_signal_assign(tree_t t, cgen_ctx_t *ctx)
{
   LLVMValueRef reject = cgen_expr(tree_reject(t), ctx);

   for (unsigned i = 0; i < tree_waveforms(t); i++) {
      tree_t w = tree_waveform(t, i);

      tree_t target = tree_target(t);
      tree_t value  = tree_value(w);

      LLVMValueRef rhs = cgen_expr(value, ctx);
      LLVMValueRef after = (tree_has_delay(w)
                            ? cgen_expr(tree_delay(w), ctx)
                            : llvm_int64(0));

      LLVMValueRef p_signal = cgen_signal_lvalue(tree_target(t), ctx);

      if (type_is_array(tree_type(value)))
         cgen_sched_waveform_vec(p_signal, tree_type(target),
                                 rhs, tree_type(value), after, reject, ctx);
      else
         cgen_sched_waveform(p_signal, rhs, after, reject, ctx);
   }
}

static void cgen_assert(tree_t t, cgen_ctx_t *ctx)
{
   int is_report = tree_attr_int(t, ident_new("is_report"), 0);

   LLVMValueRef message  = cgen_expr(tree_message(t), ctx);
   LLVMValueRef severity = cgen_expr(tree_severity(t), ctx);

   LLVMBasicBlockRef thenbb, elsebb = NULL;
   if (!is_report) {
      LLVMValueRef test = cgen_expr(tree_value(t), ctx);
      LLVMValueRef failed = LLVMBuildNot(builder, test, "");

      thenbb = LLVMAppendBasicBlock(ctx->fn, "assert_fail");
      elsebb = LLVMAppendBasicBlock(ctx->fn, "assert_pass");

      LLVMBuildCondBr(builder, failed, thenbb, elsebb);

      LLVMPositionBuilderAtEnd(builder, thenbb);
   }

   type_t msg_type = tree_type(tree_message(t));

   LLVMValueRef args[] = {
      cgen_array_data_ptr(msg_type, message),
      cgen_array_len(msg_type, 0, message),
      severity,
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), "")
   };
   LLVMBuildCall(builder, llvm_fn("_assert_fail"),
                 args, ARRAY_LEN(args), "");

   if (!is_report) {
      LLVMBuildBr(builder, elsebb);
      LLVMPositionBuilderAtEnd(builder, elsebb);
   }
}

static void cgen_if(tree_t t, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef then_bb = LLVMAppendBasicBlock(ctx->fn, "then");
   LLVMBasicBlockRef else_bb = LLVMAppendBasicBlock(ctx->fn, "else");

   LLVMBasicBlockRef end_bb =
      (tree_else_stmts(t) > 0)
      ? LLVMAppendBasicBlock(ctx->fn, "ifend")
      : else_bb;

   LLVMValueRef test = cgen_expr(tree_value(t), ctx);
   LLVMBuildCondBr(builder, test, then_bb, else_bb);

   LLVMPositionBuilderAtEnd(builder, then_bb);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), ctx);

   LLVMBuildBr(builder, end_bb);

   if (tree_else_stmts(t) > 0) {
      LLVMPositionBuilderAtEnd(builder, else_bb);

      for (unsigned i = 0; i < tree_else_stmts(t); i++)
         cgen_stmt(tree_else_stmt(t, i), ctx);

      LLVMBuildBr(builder, end_bb);
   }

   LLVMPositionBuilderAtEnd(builder, end_bb);
}

static void cgen_return(tree_t t, cgen_ctx_t *ctx)
{
   if (tree_has_value(t)) {
      LLVMValueRef rval = cgen_expr(tree_value(t), ctx);

      type_t stype = tree_type(tree_value(t));

      // If we are returning an array then wrap it with metadata
      if (type_is_array(stype)) {
         // Need to make a copy of this array as it is currently
         // on the stack

         type_t rtype = type_result(tree_type(ctx->fdecl));

         LLVMTypeRef base_type = llvm_type(type_elem(stype));

         LLVMValueRef args[] = {
            cgen_array_len(stype, -1, rval),
            llvm_sizeof(base_type)
         };
         LLVMValueRef buf = LLVMBuildCall(builder, llvm_fn("_tmp_alloc"),
                                          args, ARRAY_LEN(args), "buf");

         LLVMValueRef buf_ptr =
            LLVMBuildPointerCast(builder, buf,
                                 LLVMPointerType(base_type, 0), "buf_ptr");

         if (!cgen_const_bounds(rtype)) {
            // Returning a wrapped array

            LLVMValueRef rarray = cgen_array_meta_1(
               rtype,
               cgen_array_left(stype, rval),
               cgen_array_right(stype, rval),
               cgen_array_dir(stype, rval),
               buf_ptr);

            cgen_array_copy(stype, rtype, rval, rarray, NULL, ctx);

            LLVMBuildRet(builder, rarray);
         }
         else {
            // Returning an array of known dimension

            cgen_array_copy(stype, rtype, rval, buf_ptr, NULL, ctx);

            LLVMBuildRet(builder, buf_ptr);
         }
      }
      else
         LLVMBuildRet(builder, rval);
   }
   else {
      if (ctx->state != NULL) {
         // Free the dynamic context
         LLVMBuildFree(builder, LLVMBuildStructGEP(builder, ctx->state, 0, ""));
      }

      LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
   }

   LLVMBasicBlockRef unreach_bb = LLVMAppendBasicBlock(ctx->fn, "unreach");
   LLVMPositionBuilderAtEnd(builder, unreach_bb);
}

static void cgen_while(tree_t t, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "wbody");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "wexit");
   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "while");

   LLVMBuildBr(builder, test_bb);
   LLVMPositionBuilderAtEnd(builder, test_bb);

   if (tree_has_value(t)) {
      LLVMValueRef test = cgen_expr(tree_value(t), ctx);
      LLVMBuildCondBr(builder, test, body_bb, exit_bb);
   }
   else
      LLVMBuildBr(builder, body_bb);

   struct block_list *bl = xmalloc(sizeof(struct block_list));
   bl->exit_bb  = exit_bb;
   bl->entry_bb = test_bb;
   bl->name     = tree_ident(t);
   bl->next     = ctx->blocks;

   ctx->blocks = bl;

   LLVMPositionBuilderAtEnd(builder, body_bb);
   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), ctx);
   LLVMBuildBr(builder, test_bb);

   LLVMPositionBuilderAtEnd(builder, exit_bb);

   ctx->blocks = bl->next;
   free(bl);
}

static void cgen_block(tree_t t, cgen_ctx_t *ctx)
{
   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), ctx);
}

static void cgen_loop_control(tree_t t, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef false_bb = LLVMAppendBasicBlock(ctx->fn, "c_false");

   if (tree_has_value(t)) {
      LLVMBasicBlockRef true_bb = LLVMAppendBasicBlock(ctx->fn, "c_true");
      LLVMValueRef test = cgen_expr(tree_value(t), ctx);
      LLVMBuildCondBr(builder, test, true_bb, false_bb);
      LLVMPositionBuilderAtEnd(builder, true_bb);
   }

   ident_t label = tree_ident2(t);
   struct block_list *bl;
   for (bl = ctx->blocks; (bl != NULL) && (bl->name != label); bl = bl->next)
      ;
   assert(bl != NULL);

   LLVMBuildBr(builder, (tree_kind(t) == T_EXIT) ? bl->exit_bb : bl->entry_bb);

   LLVMPositionBuilderAtEnd(builder, false_bb);
}

static void cgen_case_scalar(tree_t t, cgen_ctx_t *ctx)
{
   // Case with scalar value maps onto LLVM case

   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "case_exit");

   LLVMBasicBlockRef else_bb = exit_bb;
   unsigned num_cases = 0;
   for (unsigned i = 0; i < tree_assocs(t); i++) {
      if (tree_assoc(t, i).kind == A_OTHERS)
         else_bb = LLVMAppendBasicBlock(ctx->fn, "case_others");
      else
         num_cases++;
   }

   LLVMValueRef val = cgen_expr(tree_value(t), ctx);
   LLVMValueRef sw = LLVMBuildSwitch(builder, val, else_bb, num_cases);

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);
      switch (a.kind) {
      case A_NAMED:
         {
            LLVMBasicBlockRef bb = LLVMAppendBasicBlock(ctx->fn, "");
            LLVMAddCase(sw, cgen_expr(a.name, ctx), bb);

            LLVMPositionBuilderAtEnd(builder, bb);
         }
         break;

      case A_OTHERS:
         LLVMPositionBuilderAtEnd(builder, else_bb);
         break;

      default:
         assert(false);
      }

      cgen_stmt(a.value, ctx);
      LLVMBuildBr(builder, exit_bb);
   }

   LLVMPositionBuilderAtEnd(builder, exit_bb);
}

static void cgen_case_array(tree_t t, cgen_ctx_t *ctx)
{
   // Case with array value must use chain of ifs

   // TODO: multiple calls to cgen_array_rel is very inefficient
   //       replace this with code to compare all values in a single
   //       loop (e.g. build a bit mask of length #assocs)

   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "case_exit");

   LLVMValueRef val = cgen_expr(tree_value(t), ctx);
   type_t type = tree_type(tree_value(t));

   bool have_others = false;
   for (unsigned i = 0; i < tree_assocs(t); i++) {
      LLVMBasicBlockRef next_bb = NULL;
      assoc_t a = tree_assoc(t, i);
      switch (a.kind) {
      case A_NAMED:
         {
            LLVMBasicBlockRef this_bb =
               LLVMAppendBasicBlock(ctx->fn, "case_body");
            next_bb = LLVMAppendBasicBlock(ctx->fn, "case_test");
            LLVMValueRef eq = cgen_array_rel(val, cgen_expr(a.name, ctx),
                                             type, type, LLVMIntEQ, ctx);
            LLVMBuildCondBr(builder, eq, this_bb, next_bb);
            LLVMPositionBuilderAtEnd(builder, this_bb);
         }
         break;

      case A_OTHERS:
         next_bb = exit_bb;
         have_others = true;
         break;

      default:
         assert(false);
      }

      cgen_stmt(a.value, ctx);
      LLVMBuildBr(builder, exit_bb);

      LLVMPositionBuilderAtEnd(builder, next_bb);
   }

   if (!have_others)
      LLVMBuildBr(builder, exit_bb);

   LLVMPositionBuilderAtEnd(builder, exit_bb);
}

static void cgen_case(tree_t t, cgen_ctx_t *ctx)
{
   if (type_is_array(tree_type(tree_value(t))))
      cgen_case_array(t, ctx);
   else
      cgen_case_scalar(t, ctx);
}

static void cgen_builtin_pcall(ident_t builtin, LLVMValueRef *args,
                               type_t *arg_types)
{
   if (icmp(builtin, "deallocate")) {
      LLVMValueRef ptr = LLVMBuildLoad(builder, args[0], "");

      type_t access = type_access(arg_types[0]);
      if (type_is_array(access) && !cgen_const_bounds(access)) {
         LLVMValueRef meta = LLVMBuildLoad(builder, ptr, "meta");

         LLVMValueRef data = cgen_array_data_ptr(access, meta);
         LLVMBuildFree(builder, data);
      }

      LLVMBuildFree(builder, ptr);
      LLVMBuildStore(builder, LLVMConstNull(LLVMTypeOf(ptr)), args[0]);
   }
   else if (icmp(builtin, "file_open1")) {
      LLVMValueRef args2[] = {
         LLVMConstNull(LLVMPointerType(LLVMInt8Type(), 0)),
         args[0],
         cgen_array_data_ptr(arg_types[1], args[1]),
         cgen_array_len(arg_types[1], 0, args[1]),
         args[2],
      };
      LLVMBuildCall(builder, llvm_fn("_file_open"),
                    args2, ARRAY_LEN(args2), "");
   }
   else if (icmp(builtin, "file_open2")) {
      LLVMValueRef args2[] = {
         args[0],
         args[1],
         cgen_array_data_ptr(arg_types[2], args[2]),
         cgen_array_len(arg_types[2], 0, args[2]),
         args[3],
      };
      LLVMBuildCall(builder, llvm_fn("_file_open"),
                    args2, ARRAY_LEN(args2), "");
   }
   else if (icmp(builtin, "file_write")) {
      LLVMValueRef data, len;
      if (type_is_array(arg_types[1])) {
         data = cgen_array_data_ptr(arg_types[1], args[1]);
         len  = cgen_array_len(arg_types[1], 0, args[1]);
      }
      else {
         LLVMTypeRef lltype = llvm_type(arg_types[1]);
         data = LLVMBuildAlloca(builder, lltype, "");
         len  = LLVMBuildIntCast(builder, LLVMSizeOf(lltype),
                                 LLVMInt32Type(), "");
         LLVMBuildStore(builder, args[1], data);
      }

      LLVMValueRef args2[] = {
         args[0],
         llvm_void_cast(data),
         len
      };
      LLVMBuildCall(builder, llvm_fn("_file_write"),
                    args2, ARRAY_LEN(args2), "");
   }
   else if (icmp(builtin, "file_read")) {
      LLVMValueRef data, inlen, outlen;
      if (type_is_array(arg_types[1])) {
         data   = cgen_array_data_ptr(arg_types[1], args[1]);
         inlen  = cgen_array_len(arg_types[1], 0, args[1]);
         outlen = args[2];
      }
      else {
         LLVMTypeRef lltype = llvm_type(arg_types[1]);
         data   = args[1];
         inlen  = LLVMBuildIntCast(builder, LLVMSizeOf(lltype),
                                 LLVMInt32Type(), "");
         outlen = LLVMConstNull(LLVMPointerType(LLVMInt32Type(), 0));
      }

      LLVMValueRef args2[] = {
         args[0],
         llvm_void_cast(data),
         inlen,
         outlen
      };
      LLVMBuildCall(builder, llvm_fn("_file_read"),
                    args2, ARRAY_LEN(args2), "");
   }
   else if (icmp(builtin, "file_close"))
      LLVMBuildCall(builder, llvm_fn("_file_close"), args, 1, "");
   else
      fatal("cannot generate code for builtin %s", istr(builtin));
}

static void cgen_pcall(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);

   const bool in_function =
      (ctx->fdecl != NULL) && (tree_kind(ctx->fdecl) == T_FUNC_BODY);

   const int nparams = tree_params(t);

   ident_t builtin = tree_attr_str(decl, ident_new("builtin"));

   if (tree_attr_int(decl, never_waits_i, 0) || in_function || builtin) {
      // Simple case where the called procedure never waits so we can ignore
      // the return value

      LLVMValueRef args[nparams + 1];
      type_t arg_types[nparams + 1];
      cgen_call_args(t, args, arg_types, ctx);

      // Final parameter to a procedure is its dynamic context
      args[nparams] = LLVMConstNull(llvm_void_ptr());

      if (builtin != NULL)
         cgen_builtin_pcall(builtin, args, arg_types);
      else {
         // Regular procedure call
         LLVMBuildCall(builder, cgen_pdecl(decl), args, nparams + 1, "");
      }
   }
   else {
      // Find the basic block to jump to when the procedure resumes
      struct proc_entry *it;
      for (it = ctx->entry_list; it && it->wait != t; it = it->next)
         ;
      assert(it != NULL);

      LLVMBuildBr(builder, it->bb);
      LLVMPositionBuilderAtEnd(builder, it->bb);

      // If we wait as a result of executing the procedure we want
      // to jump back here when awoken
      LLVMValueRef state_ptr   = LLVMBuildStructGEP(builder, ctx->state, 0, "");
      LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx->state, 1, "");
      LLVMBuildStore(builder, llvm_int32(it->state_num), state_ptr);

      LLVMValueRef args[nparams + 1];
      type_t arg_types[nparams + 1];
      cgen_call_args(t, args, arg_types, ctx);
      args[nparams] = LLVMBuildLoad(builder, context_ptr, "context");

      LLVMValueRef ret =
         LLVMBuildCall(builder, cgen_pdecl(decl), args, nparams + 1, "");

      LLVMValueRef waited = LLVMBuildICmp(builder, LLVMIntNE, ret,
                                          LLVMConstNull(llvm_void_ptr()),
                                          "waited");

      LLVMBasicBlockRef wait_bb = LLVMAppendBasicBlock(ctx->fn, "wait");
      LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(ctx->fn, "cont");

      LLVMBuildCondBr(builder, waited, wait_bb, cont_bb);

      LLVMPositionBuilderAtEnd(builder, wait_bb);

      // Stash the returned pointer in the process/procedure context
      LLVMBuildStore(builder, ret, context_ptr);

      if (ctx->proc == NULL) {
         // We are inside a procedure so return a pointer to the dynamic context
         LLVMBuildRet(builder, llvm_void_cast(ctx->state));
      }
      else
         LLVMBuildRetVoid(builder);

      LLVMPositionBuilderAtEnd(builder, cont_bb);

      // Clear the dynamic context pointer so the next procedure call
      // starts fresh
      LLVMBuildStore(builder, LLVMConstNull(llvm_void_ptr()), context_ptr);
   }
}

static void cgen_stmt(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_WAIT:
      cgen_wait(t, ctx);
      break;
   case T_VAR_ASSIGN:
      cgen_var_assign(t, ctx);
      break;
   case T_SIGNAL_ASSIGN:
      cgen_signal_assign(t, ctx);
      break;
   case T_ASSERT:
      cgen_assert(t, ctx);
      break;
   case T_IF:
      cgen_if(t, ctx);
      break;
   case T_RETURN:
      cgen_return(t, ctx);
      break;
   case T_WHILE:
      cgen_while(t, ctx);
      break;
   case T_BLOCK:
      cgen_block(t, ctx);
      break;
   case T_EXIT:
   case T_NEXT:
      cgen_loop_control(t, ctx);
      break;
   case T_CASE:
      cgen_case(t, ctx);
      break;
   case T_PCALL:
      cgen_pcall(t, ctx);
      break;
   default:
      fatal("missing cgen_stmt for %s", tree_kind_str(tree_kind(t)));
   }
}

static void cgen_jump_table_fn(tree_t t, void *arg)
{
   tree_kind_t kind = tree_kind(t);

   if ((kind != T_WAIT) && (kind != T_PCALL))
      return;

   if (kind == T_PCALL) {
      tree_t decl = tree_ref(t);
      if (tree_attr_int(decl, never_waits_i, 0))
         return;
      else if (tree_attr_str(decl, ident_new("builtin")))
         return;
   }

   cgen_ctx_t *ctx = arg;

   struct proc_entry *p = xmalloc(sizeof(struct proc_entry));
   p->next = NULL;
   p->wait = t;
   p->bb   = NULL;

   if (ctx->entry_list == NULL) {
      p->state_num = 1;
      ctx->entry_list = p;
   }
   else {
      struct proc_entry *it;
      for (it = ctx->entry_list; it->next != NULL; it = it->next)
         ;
      p->state_num = it->state_num + 1;
      it->next = p;
   }
}

struct cgen_proc_var_ctx {
   LLVMTypeRef *types;
   unsigned    offset;
};

static void cgen_visit_proc_vars(tree_t t, void *context)
{
   struct cgen_proc_var_ctx *ctx = context;

   ctx->types[ctx->offset] = llvm_type(tree_type(t));
   tree_add_attr_int(t, var_offset_i, ctx->offset);

   ctx->offset++;
}

static LLVMTypeRef cgen_process_state_type(tree_t t)
{
   unsigned nvars = tree_visit_only(t, NULL, NULL, T_VAR_DECL);

   LLVMTypeRef fields[nvars + 2];
   fields[0] = LLVMInt32Type();   // State
   fields[1] = llvm_void_ptr();   // Procedure dynamic context

   struct cgen_proc_var_ctx ctx = {
      .types  = fields,
      .offset = 2
   };
   tree_visit_only(t, cgen_visit_proc_vars, &ctx, T_VAR_DECL);

   char name[64];
   snprintf(name, sizeof(name), "%s__state_s", istr(tree_ident(t)));
   LLVMTypeRef ty = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
   if (ty == NULL)
      fatal("failed to add type name %s", name);
   LLVMStructSetBody(ty, fields, ARRAY_LEN(fields), false);

   return ty;
}

static void cgen_jump_table(cgen_ctx_t *ctx, LLVMBasicBlockRef default_bb)
{
   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget, default_bb, 10);

   LLVMAddCase(jswitch, llvm_int32(0), default_bb);

   struct proc_entry *it;
   for (it = ctx->entry_list; it != NULL; it = it->next) {
      it->bb = LLVMAppendBasicBlock(ctx->fn, istr(tree_ident(it->wait)));
      LLVMAddCase(jswitch, llvm_int32(it->state_num), it->bb);
   }
}

static void cgen_proc_var_init(tree_t t, cgen_ctx_t *ctx)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t v = tree_decl(t, i);
      if (tree_kind(v) == T_VAR_DECL) {
         assert(tree_has_value(v));
         LLVMValueRef val = cgen_expr(tree_value(v), ctx);

         type_t ty = tree_type(v);
         if (type_is_array(ty)) {
            if (!cgen_const_bounds(ty)) {
               // Also store the meta data in the state structure
               // The array data will currently be allocated on the stack so
               // copy into into a malloc-ed area so its value will be preserved
               // when the process or procedure suspends

               LLVMTypeRef elem  = llvm_type(type_elem(ty));
               LLVMValueRef len  = cgen_array_len(ty, -1, val);
               LLVMValueRef data = LLVMBuildArrayMalloc(builder, elem, len,
                                                        "proc_array");

               LLVMValueRef new_meta =
                  LLVMBuildInsertValue(builder, val, data, 0, "new_meta");

               cgen_array_copy(ty, ty, val, new_meta, NULL, ctx);

               int offset = tree_attr_int(v, var_offset_i, -1);
               assert(offset != -1);

               LLVMValueRef meta_ptr =
                  LLVMBuildStructGEP(builder, ctx->state, offset, "meta_ptr");
               LLVMBuildStore(builder, new_meta, meta_ptr);
            }
            else {
               LLVMValueRef var_ptr = cgen_get_var(v, ctx);
               cgen_array_copy(ty, ty, val, var_ptr, NULL, ctx);
            }
         }
         else
            LLVMBuildStore(builder, val, cgen_get_var(v, ctx));
      }
   }
}

static void cgen_process(tree_t t)
{
   assert(tree_kind(t) == T_PROCESS);

   struct cgen_ctx ctx = {
      .entry_list = NULL,
      .proc       = t
   };

   // Create a global structure to hold process state
   char state_name[64];
   snprintf(state_name, sizeof(state_name),
            "%s__state", istr(tree_ident(t)));
   LLVMTypeRef state_ty = cgen_process_state_type(t);
   ctx.state = LLVMAddGlobal(module, state_ty, state_name);
   LLVMSetLinkage(ctx.state, LLVMInternalLinkage);

   // Process state is initially undefined: call process function
   // with non-zero argument to initialise
   LLVMSetInitializer(ctx.state, LLVMGetUndef(state_ty));

   LLVMTypeRef pargs[] = { LLVMInt32Type() };
   LLVMTypeRef ftype = LLVMFunctionType(LLVMVoidType(), pargs, 1, false);
   ctx.fn = LLVMAddFunction(module, istr(tree_ident(t)), ftype);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(ctx.fn, "entry");
   LLVMBasicBlockRef jt_bb    = LLVMAppendBasicBlock(ctx.fn, "jump_table");
   LLVMBasicBlockRef init_bb  = LLVMAppendBasicBlock(ctx.fn, "init");
   LLVMBasicBlockRef start_bb = LLVMAppendBasicBlock(ctx.fn, "start");

   LLVMPositionBuilderAtEnd(builder, entry_bb);

   // If the parameter is non-zero jump to the init block

   LLVMValueRef param = LLVMGetParam(ctx.fn, 0);
   LLVMValueRef reset =
      LLVMBuildICmp(builder, LLVMIntNE, param, llvm_int32(0), "");
   LLVMBuildCondBr(builder, reset, init_bb, jt_bb);

   // Generate the jump table at the start of a process to handle
   // resuming from a wait statement

   LLVMPositionBuilderAtEnd(builder, jt_bb);

   tree_visit(t, cgen_jump_table_fn, &ctx);

   if (ctx.entry_list == NULL)
      warn_at(tree_loc(t), "no wait statement in process");

   cgen_jump_table(&ctx, start_bb);

   LLVMPositionBuilderAtEnd(builder, init_bb);

   // Variable initialisation

   cgen_proc_var_init(t, &ctx);

   // Return to simulation kernel after initialisation

   LLVMValueRef state_ptr   = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx.state, 1, "");

   cgen_sched_process(llvm_int64(0));
   LLVMBuildStore(builder, llvm_int32(0 /* start */), state_ptr);
   LLVMBuildStore(builder, LLVMConstNull(llvm_void_ptr()), context_ptr);
   LLVMBuildRetVoid(builder);

   // Sequential statements

   LLVMPositionBuilderAtEnd(builder, start_bb);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   LLVMBuildBr(builder, start_bb);

   // Free context memory

   while (ctx.entry_list != NULL) {
      struct proc_entry *next = ctx.entry_list->next;
      free(ctx.entry_list);
      ctx.entry_list = next;
   }
}

static LLVMValueRef cgen_signal_init(type_t type, LLVMValueRef resolution)
{
   if (type_is_array(type)) {
      range_t r = type_dim(type, 0);
      int64_t low, high;
      range_bounds(r, &low, &high);

      const unsigned n_elems = high - low + 1;

      LLVMValueRef array_init[n_elems];
      LLVMValueRef init = cgen_signal_init(type_elem(type), resolution);
      for (unsigned i = 0; i < n_elems; i++)
         array_init[i] = init;
      return LLVMConstArray(LLVMTypeOf(init), array_init, n_elems);
   }
   else {
      LLVMValueRef init[SIGNAL_N_FIELDS];
      init[SIGNAL_RESOLVED]   = llvm_int64(0);
      init[SIGNAL_LAST_VALUE] = llvm_int64(0);
      init[SIGNAL_DECL]       = LLVMConstNull(llvm_void_ptr());
      init[SIGNAL_FLAGS]      = llvm_int8(0);
      init[SIGNAL_N_SOURCES]  = llvm_int8(0);
      init[SIGNAL_OFFSET]     = LLVMConstInt(LLVMInt16Type(), 0, false);
      init[SIGNAL_SOURCES]    = LLVMConstNull(llvm_void_ptr());
      init[SIGNAL_SENSITIVE]  = LLVMConstNull(llvm_void_ptr());
      init[SIGNAL_EVENT_CB]   = LLVMConstNull(llvm_void_ptr());
      init[SIGNAL_RESOLUTION] = llvm_void_cast(resolution);
      init[SIGNAL_LAST_EVENT] = LLVMConstInt(LLVMInt64Type(), INT64_MAX, false);

      LLVMTypeRef signal_s = LLVMGetTypeByName(module, "signal_s");
      assert(signal_s != NULL);

      return LLVMConstNamedStruct(signal_s, init, ARRAY_LEN(init));
   }
}

static LLVMTypeRef cgen_signal_type(type_t type)
{
   if ((type != NULL) && type_is_array(type)) {
      LLVMTypeRef base_type = cgen_signal_type(type_elem(type));

      if (type_kind(type) == T_UARRAY)
         return llvm_uarray_type(base_type, cgen_array_dims(type));
      else {
         range_t r = type_dim(type, 0);
         int64_t low, high;
         range_bounds(r, &low, &high);

         const unsigned n_elems = high - low + 1;

         return LLVMArrayType(base_type, n_elems);
      }
   }
   else {
      LLVMTypeRef ty = LLVMGetTypeByName(module, "signal_s");
      if (ty == NULL) {
         LLVMTypeRef fields[SIGNAL_N_FIELDS];
         fields[SIGNAL_RESOLVED]   = LLVMInt64Type();
         fields[SIGNAL_LAST_VALUE] = LLVMInt64Type();
         fields[SIGNAL_DECL]       = llvm_void_ptr();
         fields[SIGNAL_FLAGS]      = LLVMInt8Type();
         fields[SIGNAL_N_SOURCES]  = LLVMInt8Type();
         fields[SIGNAL_OFFSET]     = LLVMInt16Type();
         fields[SIGNAL_SOURCES]    = llvm_void_ptr();
         fields[SIGNAL_SENSITIVE]  = llvm_void_ptr();
         fields[SIGNAL_EVENT_CB]   = llvm_void_ptr();
         fields[SIGNAL_RESOLUTION] = llvm_void_ptr();
         fields[SIGNAL_LAST_EVENT] = LLVMInt64Type();

         if (!(ty = LLVMStructCreateNamed(LLVMGetGlobalContext(), "signal_s")))
            fatal("failed to add type name signal_s");
         LLVMStructSetBody(ty, fields, ARRAY_LEN(fields), false);
      }

      return ty;
   }
}

static LLVMValueRef cgen_resolution_func(type_t type)
{
   if ((type_kind(type) != T_SUBTYPE) || !type_has_resolution(type))
      return LLVMConstNull(llvm_void_ptr());

   // Generate a wrapper function to call the type's resolution function
   // This needs to convert an array of raw 64-bit signal values to the
   // signal's LLVM type

   char name[256];
   snprintf(name, sizeof(name), "%s$resolution", istr(type_ident(type)));

   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   if (fn != NULL)
      return fn;    // Already generated wrapper

   LLVMTypeRef args[] = {
      LLVMPointerType(LLVMInt64Type(), 0),
      LLVMInt32Type()
   };

   fn = LLVMAddFunction(module, name,
                        LLVMFunctionType(LLVMInt64Type(),
                                         args, ARRAY_LEN(args), false));

   LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(builder);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   // Convert 64-bit values to array of required type
   LLVMTypeRef elem_type = llvm_type(type);
   LLVMValueRef vals =
      LLVMBuildArrayAlloca(builder,
                           elem_type,
                           LLVMGetParam(fn, 1),  // Number of elements
                           "vals");

   // Loop prelude
   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, llvm_int32(0), i);

   LLVMBasicBlockRef loop_bb = LLVMAppendBasicBlock(fn, "loop");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(fn, "exit");

   LLVMBuildBr(builder, loop_bb);
   LLVMPositionBuilderAtEnd(builder, loop_bb);

   // Loop body

   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i_loaded");

   LLVMValueRef src = LLVMBuildGEP(builder, LLVMGetParam(fn, 0),
                                   &i_loaded, 1, "src");
   LLVMValueRef dst = LLVMBuildGEP(builder, vals, &i_loaded, 1, "dst");

   LLVMValueRef src_loaded = LLVMBuildLoad(builder, src, "src_loaded");

   LLVMBuildStore(builder,
                  LLVMBuildTrunc(builder, src_loaded, elem_type, "trunc"),
                  dst);

   LLVMValueRef i_plus1  =
      LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "i_plus1");
   LLVMBuildStore(builder, i_plus1, i);

   LLVMValueRef end = LLVMBuildICmp(builder, LLVMIntEQ, i_plus1,
                                    LLVMGetParam(fn, 1), "end");

   LLVMBuildCondBr(builder, end, exit_bb, loop_bb);

   LLVMPositionBuilderAtEnd(builder, exit_bb);

   // Wrap array in meta data and call actual resolution function

   tree_t fdecl = tree_ref(type_resolution(type));
   type_t ftype = tree_type(fdecl);

   // TODO: check what standard says about left/right and direction
   LLVMValueRef left  = llvm_int32(0);
   LLVMValueRef right = LLVMBuildSub(builder, LLVMGetParam(fn, 1),
                                     llvm_int32(1), "right");
   LLVMValueRef dir   = llvm_int8(RANGE_TO);

   LLVMValueRef wrapped =
      cgen_array_meta_1(type_param(ftype, 0), left, right, dir, vals);

   const char *rfn_name = cgen_mangle_func_name(fdecl);
   LLVMValueRef rfn = LLVMGetNamedFunction(module, rfn_name);
   if (rfn == NULL) {
      // The resolution function is not visible yet e.g. because it
      // is declared in another package
      LLVMTypeRef args[tree_ports(fdecl)];
      cgen_prototype(fdecl, args, false);

      rfn = LLVMAddFunction(module, rfn_name,
                            LLVMFunctionType(elem_type, args, 1, false));
   }

   LLVMValueRef r = LLVMBuildCall(builder, rfn, &wrapped, 1, "");

   LLVMBuildRet(builder,
                LLVMBuildZExt(builder, r, LLVMInt64Type(), ""));

   LLVMPositionBuilderAtEnd(builder, saved_bb);

   return llvm_void_cast(fn);
}

static void cgen_signal(tree_t t)
{
   assert(tree_kind(t) == T_SIGNAL_DECL);

   type_t type = tree_type(t);

   LLVMTypeRef lltype = cgen_signal_type(type);
   LLVMValueRef v = LLVMAddGlobal(module, lltype, istr(tree_ident(t)));

   LLVMValueRef r = cgen_resolution_func(type);

   LLVMSetInitializer(v, cgen_signal_init(type, r));

   tree_add_attr_ptr(t, sig_struct_i, v);
}

static void cgen_func_vars(tree_t d, void *context)
{
   cgen_ctx_t *ctx = context;

   LLVMValueRef var = cgen_local_var(d, ctx);

   tree_add_attr_ptr(d, local_var_i, var);
}

static void cgen_func_constants(tree_t d, void *context)
{
   cgen_ctx_t *ctx = context;

   tree_t value = tree_value(d);
   type_t value_type = tree_type(value);
   if (type_is_array(value_type)) {
      LLVMValueRef var = cgen_expr(value, ctx);

      type_t decl_type = tree_type(d);
      if (!cgen_const_bounds(decl_type)) {
         var = cgen_array_meta_1(decl_type,
                                 cgen_array_left(value_type, var),
                                 cgen_array_right(value_type, var),
                                 cgen_array_dir(value_type, var),
                                 cgen_array_data_ptr(value_type, var));
      }

      tree_add_attr_ptr(d, local_var_i, var);
   }
}

static void cgen_func_body(tree_t t)
{
   type_t ftype = tree_type(t);

   LLVMTypeRef args[tree_ports(t)];
   cgen_prototype(t, args, false);

   type_t rtype = type_result(ftype);
   LLVMTypeRef llrtype;
   if (type_is_array(rtype) && cgen_const_bounds(rtype))
      llrtype = LLVMPointerType(llvm_type(type_elem(rtype)), 0);
   else
      llrtype = llvm_type(rtype);

   const char *mangled = cgen_mangle_func_name(t);
   LLVMValueRef fn = LLVMGetNamedFunction(module, mangled);
   if (fn == NULL) {
      fn = LLVMAddFunction(module, mangled,
                           LLVMFunctionType(llrtype, args,
                                            type_params(ftype), false));
   }

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   struct cgen_ctx ctx = {
      .entry_list = NULL,
      .proc       = NULL,
      .fdecl      = t,
      .fn         = fn
   };

   for (unsigned i = 0; i < tree_ports(t); i++) {
      tree_t p = tree_port(t, i);
      switch (tree_class(p)) {
      case C_SIGNAL:
         tree_add_attr_ptr(p, sig_struct_i, LLVMGetParam(fn, i));
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
         tree_add_attr_ptr(p, local_var_i, LLVMGetParam(fn, i));
         break;

      default:
         assert(false);
      }
   }

   tree_visit_only(t, cgen_func_constants, &ctx, T_CONST_DECL);
   tree_visit_only(t, cgen_func_vars, &ctx, T_VAR_DECL);

   for (unsigned i = 0; i < tree_stmts(t); i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   LLVMBuildUnreachable(builder);
}

static void cgen_proc_body(tree_t t)
{
   // Procedures take an extra "context" parameter which is used to support
   // suspending and resuming. If the procedure returns non-NULL then this
   // pointer should be saved, the caller should suspend, and when it
   // resumes call the procedure again with the saved pointer as the first
   // argument. If the procedure returns NULL execution continues as
   // normal.

   const int nports = tree_ports(t);
   LLVMTypeRef args[nports + 1];
   cgen_prototype(t, args, true);

   const char *mangled = cgen_mangle_func_name(t);
   LLVMValueRef fn = LLVMGetNamedFunction(module, mangled);
   if (fn == NULL) {
      fn = LLVMAddFunction(module, mangled,
                           LLVMFunctionType(llvm_void_ptr(),
                                            args, nports + 1, false));
   }

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      switch (tree_class(p)) {
      case C_SIGNAL:
         tree_add_attr_ptr(p, sig_struct_i, LLVMGetParam(fn, i));
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
         tree_add_attr_ptr(p, local_var_i, LLVMGetParam(fn, i));
         break;

      default:
         assert(false);
      }
   }

   // Generate a jump table to handle resuming from a wait statement

   struct cgen_ctx ctx = {
      .entry_list = NULL,
      .proc       = NULL,
      .fdecl      = t,
      .fn         = fn,
      .state      = NULL
   };

   tree_visit(t, cgen_jump_table_fn, &ctx);

   if (ctx.entry_list != NULL) {
      // Only allocate a dynamic context if there are no wait statements

      const int nvars = tree_visit_only(t, NULL, NULL, T_VAR_DECL);

      LLVMTypeRef fields[nvars + 2];
      fields[0] = LLVMInt32Type();   // State
      fields[1] = llvm_void_ptr();   // Called procedure dynamic context

      struct cgen_proc_var_ctx var_ctx = {
         .types  = fields,
         .offset = 2
      };
      tree_visit_only(t, cgen_visit_proc_vars, &var_ctx, T_VAR_DECL);

      LLVMTypeRef state_type = LLVMStructType(fields, ARRAY_LEN(fields), false);
      LLVMTypeRef state_ptr_type = LLVMPointerType(state_type, 0);

      LLVMBasicBlockRef init_bb   = LLVMAppendBasicBlock(fn, "init");
      LLVMBasicBlockRef resume_bb = LLVMAppendBasicBlock(fn, "resume");
      LLVMBasicBlockRef start_bb  = LLVMAppendBasicBlock(fn, "start");
      LLVMBasicBlockRef jump_bb   = LLVMAppendBasicBlock(fn, "jump");

      // We are resuming the procedure if the context is non-NULL
      LLVMValueRef resume = LLVMBuildICmp(
         builder, LLVMIntNE, LLVMGetParam(fn, nports),
         LLVMConstNull(llvm_void_ptr()), "resume");

      LLVMBuildCondBr(builder, resume, resume_bb, init_bb);

      // When resuming get the state from the final argument

      LLVMPositionBuilderAtEnd(builder, resume_bb);

      LLVMValueRef old = LLVMBuildPointerCast(
         builder, LLVMGetParam(fn, nports),
         state_ptr_type, "old");

      LLVMBuildBr(builder, jump_bb);

      // Initialise the dynamic context on the first activation

      LLVMPositionBuilderAtEnd(builder, init_bb);

      LLVMValueRef new = LLVMBuildMalloc(builder, state_type, "new");

      LLVMValueRef state_ptr   = LLVMBuildStructGEP(builder, new, 0, "");
      LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, new, 1, "");
      LLVMBuildStore(builder, llvm_int32(0 /* start */), state_ptr);
      LLVMBuildStore(builder, LLVMConstNull(llvm_void_ptr()), context_ptr);

      LLVMBuildBr(builder, jump_bb);

      // Build a jump table

      LLVMPositionBuilderAtEnd(builder, jump_bb);

      ctx.state = LLVMBuildPhi(builder, state_ptr_type, "state");
      LLVMValueRef phi_values[]   = { old,       new };
      LLVMBasicBlockRef phi_bbs[] = { resume_bb, init_bb };
      LLVMAddIncoming(ctx.state, phi_values, phi_bbs, 2);

      cgen_jump_table(&ctx, start_bb);

      LLVMPositionBuilderAtEnd(builder, start_bb);

      cgen_proc_var_init(t, &ctx);
   }
   else
      tree_visit_only(t, cgen_func_vars, &ctx, T_VAR_DECL);

   tree_visit_only(t, cgen_func_constants, &ctx, T_CONST_DECL);

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   if (ctx.state != NULL) {
      // Free any dynamically allocated arrays
      const int ndecls = tree_decls(t);
      for (int i = 0; i < ndecls; i++) {
         tree_t decl = tree_decl(t, i);
         if (tree_kind(decl) != T_VAR_DECL)
            continue;

         type_t type = tree_type(decl);
         if (type_is_array(type) && !cgen_const_bounds(type)) {
            LLVMValueRef meta = cgen_get_var(decl, &ctx);
            LLVMValueRef data = cgen_array_data_ptr(type, meta);
            LLVMBuildFree(builder, data);
         }
      }

      // Free the dynamic context
      LLVMBuildFree(builder, LLVMBuildStructGEP(builder, ctx.state, 0, ""));
   }

   LLVMBuildRet(builder, LLVMConstNull(llvm_void_ptr()));
}

static void cgen_global_const(tree_t t)
{
   if (type_is_array(tree_type(t)) && tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (cgen_is_const(value)) {
         LLVMValueRef llvalue = cgen_expr(value, NULL);
         LLVMSetValueName(llvalue, istr(tree_ident(t)));
         LLVMSetLinkage(llvalue, LLVMExternalLinkage);
         tree_add_attr_ptr(t, local_var_i, llvalue);
      }
      else {
         // The value will be generated by the reset function
         LLVMTypeRef lltype = llvm_type(tree_type(t));
         LLVMValueRef v = LLVMAddGlobal(module, lltype, istr(tree_ident(t)));
         LLVMSetInitializer(v, LLVMGetUndef(lltype));
         tree_add_attr_ptr(t, global_const_i, v);
      }
   }
}

static void cgen_reset_function(tree_t t)
{
   char name[128];
   snprintf(name, sizeof(name), "%s_reset", istr(tree_ident(t)));

   LLVMValueRef fn =
      LLVMAddFunction(module, name,
                      LLVMFunctionType(LLVMVoidType(), NULL, 0, false));

   struct cgen_ctx ctx = {
      .fn = fn
   };

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);

      if ((tree_kind(d) == T_FILE_DECL) && tree_has_value(d)) {
         // Generate implicit call to FILE_OPEN
         tree_t value = tree_value(d);
         type_t value_type = tree_type(value);
         LLVMValueRef fname = cgen_expr(value, &ctx);

         LLVMValueRef args[] = {
            LLVMConstNull(LLVMPointerType(LLVMInt8Type(), 0)),
            cgen_get_var(d, &ctx),
            cgen_array_data_ptr(value_type, fname),
            cgen_array_len(value_type, 0, fname),
            cgen_expr(tree_file_mode(d), &ctx)
         };
         LLVMBuildCall(builder, llvm_fn("_file_open"),
                       args, ARRAY_LEN(args), "");
      }

      void *global = tree_attr_ptr(d, global_const_i);
      if (global != NULL) {
         // A global constant whose value cannot be determined at
         // compile time
         LLVMValueRef init = cgen_expr(tree_value(d), &ctx);
         LLVMBuildStore(builder, init, (LLVMValueRef)global);
      }

      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      // Schedule the initial assignment to the signal
      assert(tree_has_value(d));
      LLVMValueRef val = cgen_expr(tree_value(d), &ctx);

      LLVMValueRef p_signal = cgen_array_signal_ptr(d, llvm_int32(0));

      type_t type = tree_type(d);

      if (type_is_array(type)) {
         LLVMValueRef n_elems = cgen_array_len_recur(type, val);

         LLVMValueRef args[] = {
            llvm_void_cast(p_signal),
            llvm_void_cast(val),
            n_elems,
            cgen_array_elem_size(type)
         };
         LLVMBuildCall(builder, llvm_fn("_set_initial_vec"),
                       args, ARRAY_LEN(args), "");
      }
      else {
         LLVMValueRef args[] = {
            llvm_void_cast(p_signal),
            LLVMBuildZExt(builder, val, LLVMInt64Type(), "")
         };
         LLVMBuildCall(builder, llvm_fn("_set_initial"),
                       args, ARRAY_LEN(args), "");
      }
   }

   LLVMBuildRetVoid(builder);
}

static void cgen_file_decl(tree_t t)
{
   LLVMTypeRef file_type = llvm_void_ptr();
   LLVMValueRef f = LLVMAddGlobal(module, file_type, istr(tree_ident(t)));
   LLVMSetInitializer(f, LLVMConstNull(file_type));

   tree_add_attr_ptr(t, local_var_i, f);
}

static void cgen_shared_var(tree_t t)
{
   LLVMTypeRef lltype = llvm_type(tree_type(t));
   LLVMValueRef var = LLVMAddGlobal(module, lltype, istr(tree_ident(t)));

   struct cgen_ctx ctx;
   memset(&ctx, '\0', sizeof(ctx));
   LLVMValueRef init = cgen_expr(tree_value(t), &ctx);
   LLVMSetInitializer(var, init);

   tree_add_attr_ptr(t, local_var_i, var);
}

static void cgen_top(tree_t t)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t decl = tree_decl(t, i);
      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
         cgen_signal(decl);
         break;
      case T_FUNC_BODY:
         cgen_func_body(decl);
         break;
      case T_ALIAS:
      case T_TYPE_DECL:
         break;
      case T_CONST_DECL:
         cgen_global_const(decl);
         break;
      case T_FUNC_DECL:
      case T_PROC_DECL:
         break;
      case T_PROC_BODY:
         cgen_proc_body(decl);
         break;
      case T_FILE_DECL:
         cgen_file_decl(decl);
         break;
      case T_VAR_DECL:
         cgen_shared_var(decl);
         break;
      case T_ATTR_DECL:
      case T_ATTR_SPEC:
         break;
      default:
         fatal("cannot generate code for top level declaration %s",
               tree_kind_str(tree_kind(decl)));
      }
   }

   cgen_reset_function(t);

   if (tree_kind(t) == T_ELAB) {
      for (unsigned i = 0; i < tree_stmts(t); i++)
         cgen_process(tree_stmt(t, i));
   }
}

static void optimise(void)
{
   LLVMPassManagerRef pass_mgr = LLVMCreatePassManager();

   LLVMAddPromoteMemoryToRegisterPass(pass_mgr);
   LLVMAddInstructionCombiningPass(pass_mgr);
   LLVMAddReassociatePass(pass_mgr);
   LLVMAddGVNPass(pass_mgr);
   LLVMAddCFGSimplificationPass(pass_mgr);

   LLVMRunPassManager(pass_mgr, module);
   LLVMDisposePassManager(pass_mgr);
}

static void cgen_support_fns(void)
{
   LLVMTypeRef _sched_process_args[] = { LLVMInt64Type() };
   LLVMAddFunction(module, "_sched_process",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_process_args,
                                    ARRAY_LEN(_sched_process_args),
                                    false));

   LLVMTypeRef _sched_waveform_args[] = {
      llvm_void_ptr(),
      LLVMInt64Type(),
      LLVMInt64Type(),
      LLVMInt64Type()
   };
   LLVMAddFunction(module, "_sched_waveform",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_waveform_args,
                                    ARRAY_LEN(_sched_waveform_args),
                                    false));

   LLVMTypeRef _sched_waveform_vec_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt64Type(),
      LLVMInt64Type(),
      LLVMInt1Type()
   };
   LLVMAddFunction(module, "_sched_waveform_vec",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_waveform_vec_args,
                                    ARRAY_LEN(_sched_waveform_vec_args),
                                    false));

   LLVMTypeRef _sched_event_args[] = {
      llvm_void_ptr(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_sched_event",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_event_args,
                                    ARRAY_LEN(_sched_event_args),
                                    false));

   LLVMTypeRef _set_initial_args[] = {
      llvm_void_ptr(),
      LLVMInt64Type()
   };
   LLVMAddFunction(module, "_set_initial",
                   LLVMFunctionType(LLVMVoidType(),
                                    _set_initial_args,
                                    ARRAY_LEN(_set_initial_args),
                                    false));

   LLVMTypeRef _set_initial_vec_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_set_initial_vec",
                   LLVMFunctionType(LLVMVoidType(),
                                    _set_initial_vec_args,
                                    ARRAY_LEN(_set_initial_vec_args),
                                    false));

   LLVMTypeRef _assert_fail_args[] = {
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMInt32Type(),
      LLVMInt8Type(),
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0)
   };
   LLVMAddFunction(module, "_assert_fail",
                   LLVMFunctionType(LLVMVoidType(),
                                    _assert_fail_args,
                                    ARRAY_LEN(_assert_fail_args),
                                    false));

   LLVMTypeRef _array_reverse_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_array_reverse",
                   LLVMFunctionType(LLVMVoidType(),
                                    _array_reverse_args,
                                    ARRAY_LEN(_array_reverse_args),
                                    false));

   LLVMTypeRef _vec_load_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt1Type()
   };
   LLVMAddFunction(module, "_vec_load",
                   LLVMFunctionType(LLVMVoidType(),
                                    _vec_load_args,
                                    ARRAY_LEN(_vec_load_args),
                                    false));

   LLVMTypeRef _image_args[] = {
      LLVMInt64Type(),
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMPointerType(llvm_uarray_type(LLVMInt8Type(), 1), 0)
   };
   LLVMAddFunction(module, "_image",
                   LLVMFunctionType(LLVMVoidType(),
                                    _image_args,
                                    ARRAY_LEN(_image_args),
                                    false));

   LLVMTypeRef _tmp_alloc_args[] = {
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_tmp_alloc",
                   LLVMFunctionType(llvm_void_ptr(),
                                    _tmp_alloc_args,
                                    ARRAY_LEN(_tmp_alloc_args),
                                    false));

   LLVMTypeRef _debug_out_args[] = {
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_debug_out",
                   LLVMFunctionType(LLVMVoidType(),
                                    _debug_out_args,
                                    ARRAY_LEN(_debug_out_args),
                                    false));

   LLVMTypeRef _debug_dump_args[] = {
      llvm_void_ptr(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_debug_dump",
                   LLVMFunctionType(LLVMVoidType(),
                                    _debug_dump_args,
                                    ARRAY_LEN(_debug_dump_args),
                                    false));

   LLVMTypeRef llvm_pow_args[] = {
      LLVMDoubleType(),
      LLVMDoubleType()
   };
   LLVMAddFunction(module, "llvm.pow.f64",
                   LLVMFunctionType(LLVMDoubleType(),
                                    llvm_pow_args,
                                    ARRAY_LEN(llvm_pow_args),
                                    false));

   const int widths[] = { 1, 8, 16, 32, 64 };
   for (int i = 0; i < ARRAY_LEN(widths); i++) {
      int w = widths[i];

      LLVMTypeRef llvm_memcpy_args[] = {
         LLVMPointerType(LLVMIntType(w), 0),
         LLVMPointerType(LLVMIntType(w), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      LLVMAddFunction(module, cgen_memcpy_name(w),
                      LLVMFunctionType(LLVMVoidType(),
                                       llvm_memcpy_args,
                                       ARRAY_LEN(llvm_memcpy_args),
                                       false));
   }

   LLVMTypeRef _name_attr_args[] = {
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMPointerType(llvm_uarray_type(LLVMInt8Type(), 1), 0)
   };
   LLVMAddFunction(module, "_name_attr",
                   LLVMFunctionType(LLVMVoidType(),
                                    _name_attr_args,
                                    ARRAY_LEN(_name_attr_args),
                                    false));

   LLVMTypeRef _file_open_args[] = {
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMPointerType(llvm_void_ptr(), 0),
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMInt32Type(),
      LLVMInt8Type()
   };
   LLVMAddFunction(module, "_file_open",
                   LLVMFunctionType(LLVMVoidType(),
                                    _file_open_args,
                                    ARRAY_LEN(_file_open_args),
                                    false));

   LLVMTypeRef _file_write_args[] = {
      LLVMPointerType(llvm_void_ptr(), 0),
      llvm_void_ptr(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_file_write",
                   LLVMFunctionType(LLVMVoidType(),
                                    _file_write_args,
                                    ARRAY_LEN(_file_write_args),
                                    false));

   LLVMTypeRef _file_read_args[] = {
      LLVMPointerType(llvm_void_ptr(), 0),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt32Type(), 0)
   };
   LLVMAddFunction(module, "_file_read",
                   LLVMFunctionType(LLVMVoidType(),
                                    _file_read_args,
                                    ARRAY_LEN(_file_read_args),
                                    false));

   LLVMTypeRef _file_close_args[] = {
      LLVMPointerType(llvm_void_ptr(), 0)
   };
   LLVMAddFunction(module, "_file_close",
                   LLVMFunctionType(LLVMVoidType(),
                                    _file_close_args,
                                    ARRAY_LEN(_file_close_args),
                                    false));

   LLVMTypeRef _endfile_args[] = {
      llvm_void_ptr()
   };
   LLVMAddFunction(module, "_endfile",
                   LLVMFunctionType(LLVMInt1Type(),
                                    _endfile_args,
                                    ARRAY_LEN(_endfile_args),
                                    false));

   LLVMTypeRef _bounds_fail_args[] = {
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_bounds_fail",
                   LLVMFunctionType(LLVMVoidType(),
                                    _bounds_fail_args,
                                    ARRAY_LEN(_bounds_fail_args),
                                    false));

   LLVMTypeRef memcmp_args[] = {
      llvm_void_ptr(),
      llvm_void_ptr(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "memcmp",
                   LLVMFunctionType(LLVMInt32Type(),
                                    memcmp_args,
                                    ARRAY_LEN(memcmp_args),
                                    false));
}

static void cgen_module_name(tree_t top)
{
   const char *name_str = istr(tree_ident(top));

   size_t len = strlen(name_str);
   LLVMValueRef chars[len + 1];
   llvm_str(chars, len + 1, name_str);

   mod_name = LLVMAddGlobal(module,
                            LLVMArrayType(LLVMInt8Type(), len + 1),
                            "module_name");
   LLVMSetInitializer(mod_name,
                      LLVMConstArray(LLVMInt8Type(), chars, len + 1));
   LLVMSetLinkage(mod_name, LLVMPrivateLinkage);
}

void cgen(tree_t top)
{
   var_offset_i   = ident_new("var_offset");
   local_var_i    = ident_new("local_var");
   global_const_i = ident_new("global_const");
   sig_struct_i   = ident_new("sig_struct");
   foreign_i      = ident_new("FOREIGN");
   never_waits_i  = ident_new("never_waits");

   tree_kind_t kind = tree_kind(top);
   if ((kind != T_ELAB) && (kind != T_PACK_BODY) && (kind != T_PACKAGE))
      fatal("cannot generate code for %s", tree_kind_str(kind));

   module = LLVMModuleCreateWithName(istr(tree_ident(top)));
   builder = LLVMCreateBuilder();

   cgen_module_name(top);
   cgen_support_fns();

   cgen_top(top);

   if (opt_get_int("dump-llvm"))
      LLVMDumpModule(module);

   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   optimise();

   char fname[256];
   snprintf(fname, sizeof(fname), "_%s.bc", istr(tree_ident(top)));

   FILE *f = lib_fopen(lib_work(), fname, "w");
   if (LLVMWriteBitcodeToFD(module, fileno(f), 0, 0) != 0)
      fatal("error writing LLVM bitcode");
   fclose(f);

   LLVMDisposeBuilder(builder);
   LLVMDisposeModule(module);
}
