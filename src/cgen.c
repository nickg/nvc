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
#include "rt/rt.h"
#include "rt/cover.h"

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

#define MAX_STATIC_NETS 256
#define MAX_CASE_ARCS   32

static LLVMModuleRef  module = NULL;
static LLVMBuilderRef builder = NULL;
static LLVMValueRef   mod_name = NULL;

static ident_t var_offset_i = NULL;
static ident_t local_var_i = NULL;
static ident_t global_const_i = NULL;
static ident_t sig_nets_i = NULL;
static ident_t foreign_i = NULL;
static ident_t never_waits_i = NULL;
static ident_t stmt_tag_i = NULL;

typedef struct case_arc  case_arc_t;
typedef struct case_state case_state_t;

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

// Connects case_state_t elements
struct case_arc {
   int64_t       value;
   case_state_t *next;
};

// Decision tree used for array case statements
struct case_state {
   tree_t            stmts;
   int               narcs;
   LLVMBasicBlockRef block;
   case_arc_t        arcs[MAX_CASE_ARCS];
};

typedef enum {
   PATH_NAME,
   INSTANCE_NAME
} name_attr_t;

static LLVMValueRef cgen_expr(tree_t t, cgen_ctx_t *ctx);
static void cgen_stmt(tree_t t, cgen_ctx_t *ctx);
static LLVMValueRef cgen_get_var(tree_t decl, cgen_ctx_t *ctx);
static bool cgen_const_bounds(type_t type);
static LLVMValueRef cgen_array_data_ptr(type_t type, LLVMValueRef var);
static LLVMValueRef cgen_var_lvalue(tree_t t, cgen_ctx_t *ctx);
static LLVMValueRef cgen_const_record(tree_t t, cgen_ctx_t *ctx);
static int cgen_array_dims(type_t type);
static LLVMValueRef cgen_signal_nets(tree_t decl);
static void cgen_check_bounds(tree_t t, LLVMValueRef kind, LLVMValueRef value,
                              LLVMValueRef min, LLVMValueRef max,
                              cgen_ctx_t *ctx);

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

static unsigned byte_width(type_t t)
{
   const int bits = bit_width(t);
   return (bits / 8) + ((bits % 8 != 0) ? 1 : 0);
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

   case T_FILE:
      return llvm_void_ptr();

   default:
      fatal("cannot get LLVM type for %s", type_kind_str(type_kind(t)));
   }
}

static const char *cgen_mangle_func_name(tree_t decl)
{
   static char buf[1024];
   static_printf_begin(buf, sizeof(buf));

   type_t type = tree_type(decl);

   tree_t foreign = tree_attr_tree(decl, foreign_i);
   if (foreign != NULL) {
      assert(tree_kind(foreign) == T_AGGREGATE);

      const int nassocs = tree_assocs(foreign);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(foreign, i);
         assert(tree_subkind(a) == A_POS);

         tree_t value = tree_value(a);
         assert(tree_kind(value) == T_REF);

         tree_t ch = tree_ref(value);
         assert(tree_kind(ch) == T_ENUM_LIT);

         static_printf(buf, "%c", tree_pos(ch));
      }
   }
   else {
      static_printf(buf, "%s", istr(tree_ident(decl)));
      const int nparams = type_params(type);
      for (int i = 0; i < nparams; i++) {
         type_t param = type_param(type, i);
         static_printf(buf, "$%s", istr(type_ident(param)));
      }

      if (type_kind(type) == T_FUNC)
         static_printf(buf, "^%s", istr(type_ident(type_result(type))));
   }

   return buf;
}

static bool cgen_is_const(tree_t t)
{
   if (tree_kind(t) == T_AGGREGATE) {
      bool is_const = true;
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         tree_t a = tree_assoc(t, i);
         is_const = is_const && cgen_is_const(tree_value(a));
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

static LLVMTypeRef cgen_net_id_type(void)
{
   return LLVMInt32Type();
}

static LLVMTypeRef cgen_net_map_type(type_t type)
{
   if ((type != NULL) && type_is_array(type)) {
      if (!cgen_const_bounds(type))
         return llvm_uarray_type(cgen_net_id_type(),
                                 cgen_array_dims(type));
      else {
         range_t r = type_dim(type, 0);
         int64_t low, high;
         range_bounds(r, &low, &high);

         const unsigned n_elems = high - low + 1;

         return LLVMPointerType(LLVMArrayType(cgen_net_id_type(), n_elems), 0);
      }
   }
   else
      return LLVMPointerType(LLVMArrayType(cgen_net_id_type(), 1), 0);
}

static LLVMValueRef cgen_array_meta(type_t type,
                                    LLVMValueRef params[][3],
                                    LLVMValueRef ptr)
{
   // Argument `params' is an array [dims][3] of (left, right, dir)

   LLVMTypeRef base;
   if (type == NULL)   // NULL means generic signal type
      base = cgen_net_id_type();
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

static LLVMValueRef cgen_array_dir(type_t type, int dim, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      LLVMValueRef ldim = cgen_uarray_dim(var, dim);
      return LLVMBuildExtractValue(builder, ldim, 2, "dir");
   }
   else
      return llvm_int8(type_dim(type, dim).kind);
}

static LLVMValueRef cgen_array_left(type_t type, int dim, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      LLVMValueRef ldim = cgen_uarray_dim(var, dim);
      return LLVMBuildExtractValue(builder, ldim, 0, "left");
   }
   else
      return llvm_int32(assume_int(type_dim(type, dim).left));
}

static LLVMValueRef cgen_array_right(type_t type, int dim, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      LLVMValueRef ldim = cgen_uarray_dim(var, dim);
      return LLVMBuildExtractValue(builder, ldim, 1, "right");
   }
   else
      return llvm_int32(assume_int(type_dim(type, dim).right));
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
            tree_t p = tree_param(r.left, 0);
            tree_t value = tree_value(p);
            assert(tree_kind(value) == T_REF);

            LLVMValueRef uarray;
            tree_t decl = tree_ref(value);
            if (cgen_get_class(decl) == C_SIGNAL)
               uarray = cgen_signal_nets(decl);
            else
               uarray = cgen_get_var(decl, ctx);

            kind_ll = cgen_array_dir(type, i, uarray);
         }
         else
            kind_ll = llvm_int8(r.kind);

         LLVMValueRef downto =
            LLVMBuildICmp(builder, LLVMIntEQ,
                          kind_ll, llvm_int8(RANGE_DOWNTO), "downto");

         LLVMValueRef left  = cgen_expr(r.left, ctx);
         LLVMValueRef right = cgen_expr(r.right, ctx);

         // Check validity of array bounds against index constraint
         range_t bounds = type_dim(tree_type(r.left), 0);

         LLVMValueRef bleft = cgen_expr(bounds.left, ctx);
         LLVMValueRef bright = cgen_expr(bounds.right, ctx);

         LLVMValueRef check_kind = llvm_int32(
            (bounds.kind == RANGE_TO) ? BOUNDS_INDEX_TO : BOUNDS_INDEX_DOWNTO);

         cgen_check_bounds(r.left, check_kind, left, bleft, bright, ctx);
         cgen_check_bounds(r.right, check_kind, right, bleft, bright, ctx);

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
   LLVMTypeRef int32 = LLVMInt32Type();
   value = LLVMBuildZExt(builder, value, int32, "");
   min   = LLVMBuildZExt(builder, min, int32, "");
   max   = LLVMBuildZExt(builder, max, int32, "");

   LLVMValueRef above =
      LLVMBuildICmp(builder, LLVMIntSGE, value, min, "above");
   LLVMValueRef below =
      LLVMBuildICmp(builder, LLVMIntSLE, value, max, "below");

   LLVMValueRef in = LLVMBuildAnd(builder, above, below, "in");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, in, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   LLVMValueRef args[] = {
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      value,
      min,
      max,
      kind
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_check_array_bounds(tree_t t, type_t type, int dim,
                                    LLVMValueRef array, LLVMValueRef value,
                                    cgen_ctx_t *ctx)
{
   LLVMValueRef left  = cgen_array_left(type, dim, array);
   LLVMValueRef right = cgen_array_right(type, dim, array);
   LLVMValueRef dir   = cgen_array_dir(type, dim, array);

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

static void cgen_check_array_sizes(tree_t t, type_t ltype, type_t rtype,
                                   LLVMValueRef lval, LLVMValueRef rval,
                                   cgen_ctx_t *ctx)
{
   LLVMValueRef llen = cgen_array_len(ltype, 0, lval);
   LLVMValueRef rlen = cgen_array_len(rtype, 0, rval);

   LLVMValueRef ok = LLVMBuildICmp(builder, LLVMIntEQ, llen, rlen, "ok");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, ok, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   LLVMValueRef args[] = {
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      llvm_int32(0),
      llen,
      rlen,
      llvm_int32(BOUNDS_ARRAY_SIZE)
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
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

   cgen_check_array_bounds(r.left, type, 0, array, left, ctx);
   cgen_check_array_bounds(r.right, type, 0, array, right, ctx);

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
   type_t type = tree_type(decl);

   LLVMValueRef nets = cgen_signal_nets(decl);

   if (type_is_array(type)) {
      type_t elem_type = type_elem(type);
      if (type_is_array(elem_type)) {
         LLVMValueRef stride = cgen_array_len(elem_type, 0, nets);
         elem = LLVMBuildMul(builder, elem, stride, "");
      }
   }

   const bool wrap = type_is_array(type) && !cgen_const_bounds(type);
   if (wrap) {
      // Generate a new meta-data structure offset by `elem'
      LLVMValueRef indexes[] = { elem };
      LLVMValueRef offset =
         LLVMBuildGEP(builder, LLVMBuildExtractValue(builder, nets, 0, ""),
                      indexes, ARRAY_LEN(indexes), "");
      return LLVMBuildInsertValue(builder, nets, offset, 0, "");
   }
   else {
      LLVMValueRef indexes[] = { llvm_int32(0), elem };
      return LLVMBuildGEP(builder, nets, indexes, ARRAY_LEN(indexes), "");
   }
}

static LLVMValueRef cgen_get_var(tree_t decl, cgen_ctx_t *ctx)
{
   void *local = tree_attr_ptr(decl, local_var_i);
   if (local != NULL)
      return (LLVMValueRef)local;

   void *global = tree_attr_ptr(decl, global_const_i);
   if (global != NULL) {
      type_t type = tree_type(decl);
      if (type_is_array(type) && cgen_const_bounds(type))
         return global;
      else
         return LLVMBuildLoad(builder, (LLVMValueRef)global, "global");
   }

   tree_kind_t kind = tree_kind(decl);
   assert((kind == T_VAR_DECL)
          || (kind == T_CONST_DECL)
          || (kind == T_FILE_DECL));

   type_t type = tree_type(decl);

   int offset = tree_attr_int(decl, var_offset_i, -1);
   if (offset == -1) {
      const char *name = istr(tree_ident(decl));

      LLVMValueRef var = LLVMGetNamedGlobal(module, name);
      if (var == NULL) {
         // This variable is not defined anywhere in this translation unit
         // so make it an external symbol and hope the linker fixes it up
         LLVMTypeRef lltype = llvm_type(type);
         var = LLVMAddGlobal(module, lltype, name);
         LLVMSetLinkage(var, LLVMExternalLinkage);
      }

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
   LLVMValueRef src_dir = cgen_array_dir(src_type, 0, src);
   LLVMValueRef dst_dir = cgen_array_dir(dest_type, 0, dst);

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
      const int b = byte_width(src_type);
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
   else {
      LLVMTypeRef ptr_type = LLVMPointerType(LLVMIntType(width), 0);
      src_ptr = LLVMBuildPointerCast(builder, src_ptr, ptr_type, "");
      dst_ptr = LLVMBuildPointerCast(builder, dst_ptr, ptr_type, "");
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
      LLVMValueRef init = cgen_expr(tree_value(d), ctx);
      if (type_is_array(type))
         cgen_array_copy(type, type, init, var, NULL, ctx);
      else
         LLVMBuildStore(builder, init, var);
   }

   return var;
}

static void cgen_prototype(tree_t t, LLVMTypeRef *args, bool procedure)
{
   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      type_t type = tree_type(p);

      port_mode_t mode = tree_subkind(p);
      const bool array = type_is_array(type);

      switch (tree_class(p)) {
      case C_SIGNAL:
         args[i] = cgen_net_map_type(type);
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

      case C_FILE:
         args[i] = LLVMPointerType(llvm_void_ptr(), 0);
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
   LLVMTypeRef lltype = llvm_type(tree_type(t));
   switch (tree_subkind(t)) {
   case L_INT:
      return LLVMConstInt(lltype, tree_ival(t), false);
   case L_REAL:
      return LLVMConstReal(lltype, tree_dval(t));
   case L_NULL:
      return LLVMConstNull(lltype);
   default:
      assert(false);
   }
}

static LLVMValueRef cgen_scalar_vec_load(LLVMValueRef nets, type_t type,
                                         bool last_value, cgen_ctx_t *ctx)
{
   const int bytes = byte_width(type);

   LLVMValueRef tmp = LLVMBuildAlloca(builder, llvm_type(type), "tmp");

   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      llvm_void_cast(tmp),
      llvm_int32(bytes),
      llvm_int32(0),
      llvm_int32(0),
      llvm_int1(last_value)
   };
   LLVMBuildCall(builder, llvm_fn("_vec_load"), args, ARRAY_LEN(args), "");

   return LLVMBuildLoad(builder, tmp, "");
}

static LLVMValueRef cgen_vec_load(LLVMValueRef nets, type_t type,
                                  type_t slice_type, bool last_value,
                                  cgen_ctx_t *ctx)
{
   // Copy the resolved signal into a temporary array

   const bool src_uarray = !cgen_const_bounds(type);
   const bool dst_uarray = !cgen_const_bounds(slice_type);

   LLVMValueRef fn = llvm_fn("_vec_load");

   LLVMValueRef left  = cgen_array_left(slice_type, 0, nets);
   LLVMValueRef right = cgen_array_right(slice_type, 0, nets);
   LLVMValueRef dir   = cgen_array_dir(slice_type, 0, nets);

   LLVMValueRef dir_to =
      LLVMBuildICmp(builder, LLVMIntEQ, dir, llvm_int8(RANGE_TO), "to");

   LLVMValueRef low  = LLVMBuildSelect(builder, dir_to, left, right, "");
   LLVMValueRef high = LLVMBuildSelect(builder, dir_to, right, left, "");

   LLVMValueRef low_abs  = cgen_array_off(low, nets, type, ctx, 0);
   LLVMValueRef high_abs = cgen_array_off(high, nets, type, ctx, 0);

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

   LLVMValueRef p_signal =
      src_uarray
      ? LLVMBuildExtractValue(builder, nets, 0, "")
      : nets;

   const int bytes = byte_width(type_elem(type));

   LLVMValueRef args[] = {
      llvm_void_cast(p_signal),
      llvm_void_cast(tmp),
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

static LLVMValueRef cgen_signal_nets(tree_t decl)
{
   // Return the array of nets associated with a signal
   void *nets = tree_attr_ptr(decl, sig_nets_i);
   assert(nets != NULL);
   return nets;
}

static LLVMValueRef cgen_net_flag(tree_t ref, net_flags_t flag)
{
   tree_t decl = tree_ref(ref);
   type_t type = tree_type(decl);

   LLVMValueRef nets = cgen_signal_nets(decl);

   LLVMValueRef n_elems;
   if (type_is_array(type))
      n_elems = cgen_array_len(type, 0, nets);
   else
      n_elems = llvm_int32(1);

   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      n_elems,
      llvm_int32(flag)
   };
   return LLVMBuildCall(builder, llvm_fn("_test_net_flag"),
                        args, ARRAY_LEN(args), "");
}

static LLVMValueRef cgen_last_value(tree_t signal, cgen_ctx_t *ctx)
{
   tree_t sig_decl = tree_ref(signal);
   type_t type = tree_type(sig_decl);

   LLVMValueRef nets = cgen_signal_nets(sig_decl);

   if (type_is_array(type))
      return cgen_vec_load(nets, type, type, true, ctx);
   else
      return cgen_scalar_vec_load(nets, type, true, ctx);
}

static LLVMValueRef cgen_uarray_field(tree_t expr, int field, cgen_ctx_t *ctx)
{
   LLVMValueRef meta;
   if (tree_kind(expr) == T_REF) {
      // Avoid loading all the array data if possible
      tree_t decl = tree_ref(expr);
      if (cgen_get_class(decl) == C_SIGNAL) {
         meta = cgen_signal_nets(decl);
         assert(meta != NULL);
      }
      else
         meta = cgen_get_var(decl, ctx);
   }
   else
      meta = cgen_expr(expr, ctx);

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
      tree_t p = tree_param(t, i);
      type_t type = tree_type(tree_value(p));
      class_t class = (builtin == NULL)
         ? tree_class(tree_port(decl, i))
         : C_DEFAULT;

      arg_types[i] = type;

      if ((builtin == NULL) && (class == C_SIGNAL)) {
         // Pass a pointer to the array of nets
         assert(tree_kind(tree_value(p)) == T_REF);
         tree_t sig_decl = tree_ref(tree_value(p));
         args[i] = cgen_signal_nets(sig_decl);
      }
      else {
         args[i] = NULL;

         // If this is a scalar out or inout parameter then we need
         // to pass a pointer rather than the value
         if ((builtin == NULL) || (i < nports)) {
            tree_t port = tree_port(decl, i);
            port_mode_t mode = tree_subkind(port);
            bool need_ptr = (((mode == PORT_OUT)
                              || (mode == PORT_INOUT)
                              || (class == C_FILE))
                             && !type_is_array(type));
            if (need_ptr)
               args[i] = cgen_var_lvalue(tree_value(p), ctx);
         }

         if (args[i] == NULL)
            args[i] = cgen_expr(tree_value(p), ctx);
      }

      type_t formal_type;
      if ((builtin == NULL) || (i < nports))
         formal_type = tree_type(tree_port(decl, i));
      else
         formal_type = tree_type(tree_value(p));

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
      const bool unwrap =
         type_is_array(formal_type)
         && (type_kind(formal_type) != T_UARRAY)
         && (class != C_SIGNAL)
         && (builtin == NULL);
      if (unwrap) {
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

   LLVMValueRef ldir = cgen_array_dir(left_type, 0, lhs);
   LLVMValueRef rdir = cgen_array_dir(right_type, 0, rhs);

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
      tree_t a = tree_assoc(t, i);
      LLVMValueRef this = NULL;
      switch (tree_subkind(a)) {
         case A_NAMED:
            this = cgen_expr(tree_name(a), ctx);
            break;

         case A_RANGE:
            {
               range_t r = tree_range(a);
               if ((low && r.kind == RANGE_TO)
                   || (!low && r.kind == RANGE_DOWNTO))
                  this = cgen_expr(r.left, ctx);
               else
                  this = cgen_expr(r.right, ctx);
            }
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

static LLVMValueRef cgen_name_attr(tree_t ref, type_t type, name_attr_t which)
{
   tree_t decl = tree_ref(ref);

   ident_t i = NULL;
   switch (which) {
   case PATH_NAME:
      i = tree_ident(decl);
      break;
   case INSTANCE_NAME:
      i = tree_attr_str(decl, ident_new("INSTANCE_NAME"));
      assert(i != NULL);
      break;
   }

   const char *str = istr(i);
   const size_t len = strlen(str);
   LLVMValueRef chars[len + 1];
   llvm_str(chars, len + 1, str);

   LLVMValueRef global = LLVMAddGlobal(module,
                                     LLVMArrayType(LLVMInt8Type(), len + 1),
                                     "name_attr");
   LLVMSetInitializer(global,
                      LLVMConstArray(LLVMInt8Type(), chars, len + 1));
   LLVMSetLinkage(global, LLVMPrivateLinkage);

   LLVMValueRef ptr =
      LLVMBuildPointerCast(builder, global,
                           LLVMPointerType(LLVMInt8Type(), 0), "");

   return cgen_array_meta_1(type, llvm_int32(1), llvm_int32(len),
                            llvm_int8(RANGE_TO), ptr);
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
   tree_t decl = tree_ref(t);
   type_t type = tree_type(decl);

   LLVMValueRef nets = cgen_signal_nets(decl);

   LLVMValueRef n_elems;
   if (type_is_array(type))
      n_elems = cgen_array_len(type, 0, nets);
   else
      n_elems = llvm_int32(1);

   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      n_elems
   };
   return LLVMBuildCall(builder, llvm_fn("_last_event"),
                        args, ARRAY_LEN(args), "");
}

static void cgen_widen(type_t result, LLVMValueRef *args, int nargs)
{
   // Ensure all arguments to arithmetic operators are the same width

   int max_width = 0;
   for (int i = 0; i < nargs; i++) {
      const int width = LLVMGetIntTypeWidth(LLVMTypeOf(args[i]));
      max_width = MAX(max_width, width);
   }

   LLVMTypeRef promote_type = LLVMIntType(max_width);

   for (int i = 0; i < nargs; i++) {
      LLVMTypeRef arg_type = LLVMTypeOf(args[i]);
      if (LLVMGetIntTypeWidth(arg_type) < max_width)
         args[i] = LLVMBuildIntCast(builder, args[i], promote_type, "widen");
   }
}

static LLVMValueRef cgen_narrow(type_t result, LLVMValueRef value)
{
   // Resize arithmetic result to width of target type

   LLVMTypeRef target_type = llvm_type(result);
   const int target_width = LLVMGetIntTypeWidth(target_type);
   const int actual_width = LLVMGetIntTypeWidth(LLVMTypeOf(value));

   if (target_width != actual_width)
      return LLVMBuildIntCast(builder, value, target_type, "narrow");
   else
      return value;
}

static LLVMValueRef cgen_bit_shift(bit_shift_kind_t kind, LLVMValueRef array,
                                   type_t type, LLVMValueRef shift)
{
   LLVMValueRef result = LLVMBuildAlloca(builder,
                                         llvm_uarray_type(LLVMInt1Type(), 1),
                                         "bit_shift");

   LLVMValueRef args[] = {
      llvm_int32(kind),
      cgen_array_data_ptr(type, array),
      cgen_array_len(type, 0, array),
      cgen_array_dir(type, 0, array),
      shift,
      result
   };
   LLVMBuildCall(builder, llvm_fn("_bit_shift"), args, ARRAY_LEN(args), "");

   return LLVMBuildLoad(builder, result, "");
}

static LLVMValueRef cgen_division(LLVMValueRef num, LLVMValueRef denom,
                                  tree_t t, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef zero_bb = LLVMAppendBasicBlock(ctx->fn, "div_zero");
   LLVMBasicBlockRef ok_bb   = LLVMAppendBasicBlock(ctx->fn, "div_ok");

   LLVMValueRef zero = LLVMConstInt(LLVMTypeOf(denom), 0, false);
   LLVMValueRef div_by_zero =
      LLVMBuildICmp(builder, LLVMIntEQ, denom, zero, "div0");

   LLVMBuildCondBr(builder, div_by_zero, zero_bb, ok_bb);

   LLVMPositionBuilderAtEnd(builder, zero_bb);

   LLVMValueRef args[] = {
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), "")
   };
   LLVMBuildCall(builder, llvm_fn("_div_zero"), args, ARRAY_LEN(args), "");
   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, ok_bb);
   return LLVMBuildSDiv(builder, num, denom, "");
}

static LLVMValueRef cgen_fcall(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   ident_t builtin = tree_attr_str(decl, ident_new("builtin"));

   // Special attributes
   if (builtin) {
      tree_t p0 = tree_value(tree_param(t, 0));

      if (icmp(builtin, "event"))
         return cgen_net_flag(p0, NET_F_EVENT);
      else if (icmp(builtin, "active"))
         return cgen_net_flag(p0, NET_F_ACTIVE);
      else if (icmp(builtin, "last_value"))
         return cgen_last_value(p0, ctx);
      else if (icmp(builtin, "agg_low"))
         return cgen_agg_bound(p0, true, INT32_MAX, ctx);
      else if (icmp(builtin, "agg_high"))
         return cgen_agg_bound(p0, false, INT32_MIN, ctx);
      else if (icmp(builtin, "instance_name"))
         return cgen_name_attr(p0, tree_type(t), INSTANCE_NAME);
      else if (icmp(builtin, "path_name"))
         return cgen_name_attr(p0, tree_type(t), PATH_NAME);
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

   type_t rtype = tree_has_type(t) ? tree_type(t) : NULL;

   // Regular builtin functions
   if (builtin) {
      assert(nparams > 0);
      const bool real = cgen_is_real(arg_types[0]);

      if (icmp(builtin, "mul")) {
         if (real)
            return LLVMBuildFMul(builder, args[0], args[1], "");
         else {
            cgen_widen(rtype, args, nparams);
            LLVMValueRef r = LLVMBuildMul(builder, args[0], args[1], "");
            return cgen_narrow(rtype, r);
         }
      }
      else if (icmp(builtin, "add")) {
         if (real)
            return LLVMBuildFAdd(builder, args[0], args[1], "");
         else {
            cgen_widen(rtype, args, nparams);
            LLVMValueRef r = LLVMBuildAdd(builder, args[0], args[1], "");
            return cgen_narrow(rtype, r);
         }
      }
      else if (icmp(builtin, "sub")) {
         if (real)
            return LLVMBuildFSub(builder, args[0], args[1], "");
         else {
            cgen_widen(rtype, args, nparams);
            LLVMValueRef r = LLVMBuildSub(builder, args[0], args[1], "");
            return cgen_narrow(rtype, r);
         }
      }
      else if (icmp(builtin, "div")) {
         if (real)
            return LLVMBuildFDiv(builder, args[0], args[1], "");
         else {
            cgen_widen(rtype, args, nparams);
            LLVMValueRef r = cgen_division(args[0], args[1],
                                           tree_param(t, 1), ctx);
            return cgen_narrow(rtype, r);
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
               llvm_type(rtype),
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
            llvm_int32(tree_index(tree_value(tree_param(t, 0)))),
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
         range_t r = type_dim(rtype, 0);
         int dir = (r.kind == RANGE_TO ? -1 : 1);
         return LLVMBuildAdd(builder, args[0],
                             LLVMConstInt(llvm_type(arg_types[0]), dir, false),
                             "leftof");
      }
      else if (icmp(builtin, "rightof")) {
         range_t r = type_dim(rtype, 0);
         int dir = (r.kind == RANGE_TO ? 1 : -1);
         return LLVMBuildAdd(builder, args[0],
                             LLVMConstInt(llvm_type(arg_types[0]), dir, false),
                             "rightof");
      }
      else if (icmp(builtin, "length")) {
         assert(!cgen_const_bounds(arg_types[1]));
         return cgen_array_len(arg_types[1],
                               assume_int(tree_value(tree_param(t, 0))) - 1,
                               args[1]);
      }
      else if (icmp(builtin, "uarray_dircmp")) {
         LLVMValueRef dir_eq = LLVMBuildICmp(
            builder, LLVMIntEQ,
            cgen_array_dir(arg_types[0], 0 /* XXX */, args[0]),
            LLVMBuildIntCast(builder, args[1], LLVMInt8Type(), ""),
            "diff_eq");
         LLVMValueRef neg = LLVMBuildNeg(builder, args[2], "neg");
         return LLVMBuildSelect(builder, dir_eq, args[2], neg, "dirmul");
      }
      else if (icmp(builtin, "alt"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_value(tree_param(t, 1))),
                               LLVMIntSLT, ctx);
      else if (icmp(builtin, "agt"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_value(tree_param(t, 1))),
                               LLVMIntSGT, ctx);
      else if (icmp(builtin, "aleq"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_value(tree_param(t, 1))),
                               LLVMIntSLE, ctx);
      else if (icmp(builtin, "ageq"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_value(tree_param(t, 1))),
                               LLVMIntSGE, ctx);
      else if (icmp(builtin, "endfile"))
         return LLVMBuildCall(builder, llvm_fn("_endfile"), args, 1, "");
      else if (icmp(builtin, "sll"))
         return cgen_bit_shift(BIT_SHIFT_SLL, args[0], arg_types[0], args[1]);
      else if (icmp(builtin, "srl"))
         return cgen_bit_shift(BIT_SHIFT_SRL, args[0], arg_types[0], args[1]);
      else if (icmp(builtin, "sla"))
         return cgen_bit_shift(BIT_SHIFT_SLA, args[0], arg_types[0], args[1]);
      else if (icmp(builtin, "sra"))
         return cgen_bit_shift(BIT_SHIFT_SRA, args[0], arg_types[0], args[1]);
      else if (icmp(builtin, "rol"))
         return cgen_bit_shift(BIT_SHIFT_ROL, args[0], arg_types[0], args[1]);
      else if (icmp(builtin, "ror"))
         return cgen_bit_shift(BIT_SHIFT_ROR, args[0], arg_types[0], args[1]);
      else if (icmp(builtin, "pos"))
         return LLVMBuildZExt(builder, args[0], llvm_type(rtype), "pos");
      else if (icmp(builtin, "val")) {
         const int max = type_enum_literals(rtype) - 1;
         cgen_check_bounds(t, llvm_int32(BOUNDS_ENUM), args[0],
                           llvm_int32(0), llvm_int32(max), ctx);
         return LLVMBuildIntCast(builder, args[0], llvm_type(rtype), "val");
      }
      else
         fatal("cannot generate code for builtin %s", istr(builtin));
   }
   else {
      return LLVMBuildCall(builder, cgen_fdecl(decl), args, nparams, "");
   }
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
      needs_load = (((tree_subkind(decl) == PORT_INOUT)
                     || (tree_class(decl) == C_FILE))
                    && !type_is_array(tree_type(decl)));
      // Fall-through

   case T_SIGNAL_DECL:
      if (cgen_get_class(decl) == C_SIGNAL) {
         type_t type = tree_type(decl);
         LLVMValueRef nets = cgen_signal_nets(decl);
         if (type_is_array(type))
            return cgen_vec_load(nets, type, type, false, ctx);
         else
            return cgen_scalar_vec_load(nets, type, false, ctx);
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
         array = cgen_signal_nets(decl);
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
      tree_t p = tree_param(t, i);
      assert(tree_subkind(p) == P_POS);
      LLVMValueRef offset = cgen_expr(tree_value(p), ctx);

      cgen_check_array_bounds(tree_value(p), type, i, array, offset, ctx);

      if (i > 0) {
         LLVMValueRef stride = cgen_array_len(type, i, array);
         idx = LLVMBuildMul(builder, idx, stride, "stride");
      }

      idx = LLVMBuildAdd(builder, idx,
                         cgen_array_off(offset, array, type, ctx, i), "idx");
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

         type_t elem_type = type_elem(type);
         if (type_is_array(elem_type)) {
            LLVMValueRef stride = cgen_array_len(elem_type, 0, array);
            idx = LLVMBuildMul(builder, idx, stride, "");
         }

         LLVMValueRef nets;
         if (type_kind(type) == T_UARRAY) {
            // Unwrap array to nets array
            array = LLVMBuildExtractValue(builder, array, 0, "aptr");

            LLVMValueRef indexes[] = { idx };
            nets = LLVMBuildGEP(builder, array,
                                indexes, ARRAY_LEN(indexes), "");
         }
         else {
            LLVMValueRef indexes[] = { llvm_int32(0), idx };
            nets = LLVMBuildGEP(builder, array,
                                indexes, ARRAY_LEN(indexes), "");
         }

         if (type_is_array(elem_type)) {
            // Load this sub-array into a temporary variable
            return cgen_vec_load(nets, elem_type, elem_type, false, ctx);
         }
         else
            return cgen_scalar_vec_load(nets, elem_type, false, ctx);
      }

   default:
      assert(false);
   }
}

static LLVMValueRef cgen_array_slice(tree_t t, cgen_ctx_t *ctx)
{
   tree_t value = tree_value(t);

   if (tree_kind(value) == T_REF) {
      tree_t decl = tree_ref(value);
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
         return cgen_vec_load(cgen_signal_nets(decl), type, tree_type(t),
                              false, ctx);

      default:
         assert(false);
      }
   }
   else {
      LLVMValueRef src = cgen_expr(value, ctx);
      return cgen_get_slice(src, tree_type(value), tree_range(t), ctx);
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

static LLVMValueRef *cgen_const_aggregate(tree_t t, type_t type, int dim,
                                          int *n_elems, cgen_ctx_t *ctx)
{
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

   for (int i = 0; i < *n_elems; i++)
      vals[i] = NULL;

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      tree_t value = tree_value(a);

      LLVMValueRef *sub;
      int nsub;
      if (dim < type_dims(type) - 1)
         sub = cgen_const_aggregate(value, type, dim + 1, &nsub, ctx);
      else if (tree_kind(value) == T_AGGREGATE) {
         sub  = xmalloc(sizeof(LLVMValueRef));
         nsub = 1;

         type_t sub_type = tree_type(value);
         if (type_is_array(sub_type)) {
            int nvals;
            LLVMValueRef *v = cgen_const_aggregate(value, sub_type,
                                                   0, &nvals, ctx);
            LLVMTypeRef ltype = llvm_type(type_elem(sub_type));

            *sub = LLVMConstArray(ltype, v, nvals);
         }
         else if (type_kind(sub_type) == T_RECORD) {
            *sub = cgen_const_record(value, ctx);
         }
         else
            assert(false);
      }
      else {
         sub  = xmalloc(sizeof(LLVMValueRef));
         *sub = cgen_expr(value, ctx);
         nsub = 1;
      }

      range_t r = type_dim(type, dim);

      int64_t low, high;
      range_bounds(r, &low, &high);

      switch (tree_subkind(a)) {
      case A_POS:
         if (r.kind == RANGE_TO)
            cgen_copy_vals(vals + (i * nsub), sub, nsub, false);
         else
            cgen_copy_vals(vals + ((*n_elems - i - 1) * nsub),
                           sub, nsub, true);
         break;

      case A_NAMED:
         cgen_copy_vals(vals + ((assume_int(tree_name(a)) - low) * nsub),
                        sub, nsub, false);
         break;

      case A_OTHERS:
         assert((*n_elems % nsub) == 0);
         for (int j = 0; j < (*n_elems / nsub); j++) {
            if (vals[j * nsub] == NULL)
               cgen_copy_vals(vals + (j * nsub), sub, nsub, false);
         }
         break;

      case A_RANGE:
         {
            int64_t r_low, r_high;
            range_bounds(tree_range(a), &r_low, &r_high);

            for (int j = r_low; j <= r_high; j++)
               cgen_copy_vals(vals + ((j - low) * nsub), sub, nsub, false);
         }
         break;
      }

      free(sub);
   }

   for (int i = 0; i < *n_elems; i++)
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

   const int ndims = cgen_array_dims(type);
   LLVMValueRef stride = llvm_int32(1);
   for (int i = 1; i < ndims; i++) {
      LLVMValueRef sub_len = cgen_array_len(type, i, a);
      stride = LLVMBuildMul(builder, sub_len, stride, "stride");
   }

   LLVMValueRef def = NULL;
   const int nassocs = tree_assocs(t);
   type_t assoc_type = NULL;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      tree_t value = tree_value(a);
      assoc_type = tree_type(value);
      if (tree_subkind(a) == A_OTHERS) {
         def = cgen_expr(value, ctx);
         break;
      }
   }

   LLVMBuildBr(builder, test_bb);

   if (def == NULL) {
      LLVMTypeRef lltype = llvm_type(assoc_type);
      def = LLVMGetUndef(type_is_array(assoc_type)
                         ? LLVMPointerType(lltype, 0) : lltype);
   }

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
      tree_t a = tree_assoc(t, i);
      switch (tree_subkind(a)) {
      case A_POS:
         {
            LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ, i_loaded,
                                            llvm_int32(tree_pos(a)), "");
            what = LLVMBuildSelect(builder, eq, cgen_expr(tree_value(a), ctx),
                                   what, "");
         }
         break;

      case A_NAMED:
         {
            LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ, i_loaded,
                                            cgen_expr(tree_name(a), ctx), "");
            what = LLVMBuildSelect(builder, eq, cgen_expr(tree_value(a), ctx),
                                   what, "");
         }
         break;

      case A_RANGE:
         {
            range_t r = tree_range(a);
            LLVMIntPredicate lpred =
               (r.kind == RANGE_TO ? LLVMIntSGE : LLVMIntSLE);
            LLVMIntPredicate rpred =
               (r.kind == RANGE_TO ? LLVMIntSLE : LLVMIntSGE);

            LLVMValueRef lcmp =
               LLVMBuildICmp(builder, lpred, i_loaded,
                             cgen_expr(r.left, ctx), "lcmp");
            LLVMValueRef rcmp =
               LLVMBuildICmp(builder, rpred, i_loaded,
                             cgen_expr(r.right, ctx), "rcmp");
            LLVMValueRef in = LLVMBuildOr(builder, lcmp, rcmp, "in");

            what = LLVMBuildSelect(builder, in, cgen_expr(tree_value(a), ctx),
                                   what, "");
         }
         break;

      case A_OTHERS:
         break;
      }
   }

   if (type_is_array(assoc_type))
      cgen_array_copy(assoc_type, type, what, a, i_loaded, ctx);
   else {
      LLVMValueRef indexes[] = { i_loaded };
      LLVMValueRef ptr = LLVMBuildGEP(builder, data, indexes,
                                      ARRAY_LEN(indexes), "ptr");
      LLVMBuildStore(builder, what, ptr);
   }

   LLVMValueRef inc =
      LLVMBuildAdd(builder, i_loaded, stride, "inc");
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
      tree_t a = tree_assoc(t, i);

      tree_t value = tree_value(a);
      type_t value_type = tree_type(value);
      LLVMValueRef v;
      if (type_is_array(value_type)) {
         int nvals;
         LLVMValueRef *vals =
            cgen_const_aggregate(value, value_type, 0, &nvals, ctx);
         LLVMTypeRef ltype = llvm_type(type_elem(value_type));
         v = LLVMConstArray(ltype, vals, nvals);
      }
      else
         v = cgen_expr(value, ctx);

      switch (tree_subkind(a)) {
      case A_POS:
         vals[tree_pos(a)] = v;
         break;

      case A_NAMED:
         {
            int index = cgen_field_index(type, tree_ident(tree_name(a)));
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
         int nvals;
         LLVMValueRef *vals = cgen_const_aggregate(t, type, 0, &nvals, ctx);

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
   assert(tree_params(t) == 2);
   tree_t args[] = {
      tree_value(tree_param(t, 0)),
      tree_value(tree_param(t, 1))
   };

   LLVMValueRef args_ll[] = {
      cgen_expr(args[0], ctx),
      cgen_expr(args[1], ctx)
   };

   type_t type = tree_type(t);
   LLVMValueRef var;
   if (type_kind(type) == T_UARRAY) {
      LLVMValueRef args_len[] = {
         cgen_array_len(tree_type(args[0]), 0, args_ll[0]),
         cgen_array_len(tree_type(args[1]), 0, args_ll[1])
      };
      LLVMValueRef len = LLVMBuildAdd(builder, args_len[0],
                                      args_len[1], "concat_len");

      LLVMValueRef data =
         LLVMBuildArrayAlloca(builder, llvm_type(type_elem(type)),
                              len, "concat_data");

      LLVMValueRef dims[1][3] = {
         {
            llvm_int32(1),
            len,
            llvm_int8(RANGE_TO)
         }
      };

      var = cgen_array_meta(type, dims, data);
   }
   else
      var = cgen_tmp_var(tree_type(t), "concat", ctx);

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
   tree_t value = tree_value(tree_param(t, 0));

   type_t from = tree_type(value);
   type_t to   = tree_type(t);

   type_kind_t from_k = type_kind(type_base_recur(from));
   type_kind_t to_k   = type_kind(type_base_recur(to));

   LLVMValueRef value_ll = cgen_expr(value, ctx);

   if ((from_k == T_REAL) && (to_k == T_INTEGER))
      return LLVMBuildFPToSI(builder, value_ll, llvm_type(to), "");
   else if ((from_k == T_INTEGER) && (to_k == T_REAL))
      return LLVMBuildSIToFP(builder, value_ll, llvm_type(to), "");
   else if (!cgen_const_bounds(to)) {
      // Need to wrap in metadata
      return cgen_array_meta_1(
         to,
         cgen_array_left(from, 0, value_ll),
         cgen_array_right(from, 0, value_ll),
         cgen_array_dir(from, 0, value_ll),
         cgen_array_data_ptr(from, value_ll));
   }
   else if ((from_k == T_INTEGER) && (to_k == T_INTEGER))
      // Possibly change width
      return LLVMBuildIntCast(builder, value_ll, llvm_type(to), "");
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

   tree_t value = tree_value(t);
   type_t value_type = tree_type(value);

   LLVMValueRef ptr = LLVMBuildMalloc(builder, llvm_type(type), "");

   LLVMValueRef init = cgen_expr(value, ctx);

   if (type_is_array(type) && !cgen_const_bounds(type)) {
      // Need to allocate memory for both the array and its metadata

      range_t r = type_dim(value_type, 0);

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

      cgen_array_copy(value_type, type, init, meta, NULL, ctx);
   }
   else if (type_is_array(type))
      cgen_array_copy(value_type, value_type, init, ptr, NULL, ctx);
   else
      LLVMBuildStore(builder, init, ptr);

   return ptr;
}

static LLVMValueRef cgen_all(tree_t t, cgen_ctx_t *ctx)
{
   LLVMValueRef ptr = cgen_expr(tree_value(t), ctx);

   LLVMValueRef null = LLVMBuildICmp(builder, LLVMIntEQ, ptr,
                                     LLVMConstNull(LLVMTypeOf(ptr)), "null");

   LLVMBasicBlockRef null_bb = LLVMAppendBasicBlock(ctx->fn, "all_null");
   LLVMBasicBlockRef ok_bb   = LLVMAppendBasicBlock(ctx->fn, "all_ok");

   LLVMBuildCondBr(builder, null, null_bb, ok_bb);

   LLVMPositionBuilderAtEnd(builder, null_bb);

   LLVMValueRef args[] = {
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), "")
   };
   LLVMBuildCall(builder, llvm_fn("_null_deref"), args, ARRAY_LEN(args), "");
   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, ok_bb);

   type_t type = tree_type(t);
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

static void cgen_sched_event(tree_t on, cgen_ctx_t *ctx)
{
   tree_kind_t expr_kind = tree_kind(on);
   if ((expr_kind != T_REF) && (expr_kind != T_ARRAY_REF)
       && (expr_kind != T_ARRAY_SLICE)) {
      // It is possible for constant folding to replace a signal with
      // a constant which will then appear in a sensitivity list so
      // just ignore it
      return;
   }

   tree_t decl = NULL;
   switch (expr_kind) {
   case T_REF:
      decl = tree_ref(on);
      break;

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      decl = tree_ref(tree_value(on));
      break;

   default:
      assert(false);
   }

   tree_kind_t kind = tree_kind(decl);
   if ((kind != T_SIGNAL_DECL) && (kind != T_PORT_DECL)) {
      // As above, a port could have been rewritten to reference a
      // constant declaration or enumeration literal, in which case
      // just ignore it too
      return;
   }

   type_t type = tree_type(decl);

   LLVMValueRef nets = cgen_signal_nets(decl);

   bool sequential = false;
   LLVMValueRef n_elems = NULL;
   if (expr_kind == T_REF) {
      const bool array = type_is_array(type);

      if (array)
         n_elems = cgen_array_len_recur(type, nets);
      else
         n_elems = llvm_int32(1);

      if (array && !cgen_const_bounds(type)) {
         // Unwrap the meta-data to get nets array
         nets = LLVMBuildExtractValue(builder, nets, 0, "");
      }

      // Try to optimise the case where the list of nets is sequential
      // and known at compile time
      if ((kind == T_SIGNAL_DECL) && array) {
         const int nnets = tree_nets(decl);
         int i;
         netid_t last = -1;
         for (i = 0; i < nnets; i++) {
            const netid_t nid = tree_net(decl, i);
            if ((last == -1) || (nid == last + 1))
               last = nid;
            else
               break;
         }

         sequential = (i == nnets);
      }
   }
   else {
      assert(type_is_array(type));

      LLVMValueRef index = NULL;
      switch (expr_kind) {
      case T_ARRAY_REF:
         {
            tree_t p = tree_param(on, 0);
            index = cgen_expr(tree_value(p), ctx);
            cgen_check_array_bounds(tree_value(p), type, 0, nets, index, ctx);

            n_elems = llvm_int32(1);
         }
         break;

      case T_ARRAY_SLICE:
         {
            range_t r = tree_range(on);

            LLVMValueRef left  = cgen_expr(r.left, ctx);
            LLVMValueRef right = cgen_expr(r.right, ctx);

            LLVMValueRef low  = (r.kind == RANGE_TO) ? left : right;
            LLVMValueRef high = (r.kind == RANGE_TO) ? right : left;

            cgen_check_array_bounds(r.left, type, 0, nets, left, ctx);
            cgen_check_array_bounds(r.right, type, 0, nets, right, ctx);

            index = low;
            n_elems = LLVMBuildAdd(builder,
                                   LLVMBuildSub(builder, high, low, ""),
                                   llvm_int32(1),
                                   "n_elems");
         }
         break;

      default:
         assert(false);
      }

      LLVMValueRef offset = cgen_array_off(index, nets, type, ctx, 0);

      if (type_kind(type) == T_UARRAY) {
         // Unwrap meta-data to get actual nets array
         LLVMValueRef sub_nets =
            LLVMBuildExtractValue(builder, nets, 0, "sub_nets");

         LLVMValueRef indexes[] = { offset };
         nets = LLVMBuildGEP(builder, sub_nets,
                                   indexes, ARRAY_LEN(indexes), "");
      }
      else {
         LLVMValueRef indexes[] = { llvm_int32(0), offset };
         nets = LLVMBuildGEP(builder, nets,
                             indexes, ARRAY_LEN(indexes), "");
      }
   }

   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      n_elems,
      llvm_int1(sequential)
   };
   LLVMBuildCall(builder, llvm_fn("_sched_event"),
                 args, ARRAY_LEN(args), "");
}

static void cgen_wait(tree_t t, cgen_ctx_t *ctx)
{
   if (tree_has_delay(t))
      cgen_sched_process(cgen_expr(tree_delay(t), ctx));

   const int ntriggers = tree_triggers(t);
   for (int i = 0; i < ntriggers; i++)
      cgen_sched_event(tree_trigger(t, i), ctx);

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

         LLVMValueRef var = cgen_var_lvalue(tree_value(t), ctx);

         LLVMValueRef idx = llvm_int32(0);
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_param(t, i);
            assert(tree_subkind(p) == P_POS);

            LLVMValueRef off = cgen_expr(tree_value(p), ctx);

            cgen_check_array_bounds(tree_value(p), type, i, var, off, ctx);

            if (i > 0) {
               LLVMValueRef stride = cgen_array_len(type, i, var);
               idx = LLVMBuildMul(builder, idx, stride, "stride");
            }

            idx = LLVMBuildAdd(builder, idx,
                               cgen_array_off(off, var, type, ctx, i),
                               "idx");
         }

         LLVMValueRef data = cgen_array_data_ptr(type, var);
         return LLVMBuildGEP(builder, data, &idx, 1, "");
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
   tree_t value = tree_value(t);
   type_t value_type = tree_type(value);

   LLVMValueRef rhs = cgen_expr(value, ctx);

   tree_t target = tree_target(t);
   type_t target_type = tree_type(target);

   type_kind_t base_k = type_kind(type_base_recur(target_type));
   if (base_k == T_INTEGER) {
      range_t r = type_dim(target_type, 0);
      LLVMValueRef min =
         cgen_expr((r.kind == RANGE_TO) ? r.left : r.right, ctx);
      LLVMValueRef max =
         cgen_expr((r.kind == RANGE_TO) ? r.right : r.left, ctx);
      LLVMValueRef kind = llvm_int32((r.kind == RANGE_TO)
                                     ? BOUNDS_TYPE_TO : BOUNDS_TYPE_DOWNTO);
      cgen_check_bounds(value, kind, rhs, min, max, ctx);
   }

   if (type_is_universal(value_type) && !cgen_is_real(value_type))
      rhs = LLVMBuildIntCast(builder, rhs, llvm_type(target_type), "");

   LLVMValueRef lhs = cgen_var_lvalue(target, ctx);

   if (type_is_array(target_type))
      cgen_array_copy(value_type, target_type, rhs, lhs, NULL, ctx);
   else
      LLVMBuildStore(builder, rhs, lhs);
}

static LLVMValueRef cgen_signal_lvalue(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_REF:
      return cgen_array_signal_ptr(tree_ref(t), llvm_int32(0));

   case T_ARRAY_REF:
      {
         tree_t p = tree_param(t, 0);
         assert(tree_subkind(p) == P_POS);

         type_t type = tree_type(tree_value(t));

         LLVMValueRef idx = cgen_expr(tree_value(p), ctx);

         if (tree_kind(tree_value(t)) == T_REF) {
            if (type_kind(type) == T_UARRAY) {
               tree_t decl = tree_ref(tree_value(t));
               assert(type_is_array(tree_type(decl)));

               LLVMValueRef meta = cgen_signal_nets(decl);

               cgen_check_array_bounds(tree_value(p), type, 0, meta, idx, ctx);

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

               cgen_check_array_bounds(tree_value(p), type, 0, NULL, idx, ctx);

               return cgen_array_signal_ptr(decl, off);
            }
         }
         else {
            assert(type_kind(type) != T_UARRAY);

            cgen_check_array_bounds(tree_value(p), type, 0, NULL, idx, ctx);

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

         range_t r = tree_range(t);

         LLVMValueRef left  = cgen_expr(r.left, ctx);
         LLVMValueRef right = cgen_expr(r.right, ctx);

         type_t val_type = tree_type(tree_value(t));

         cgen_check_array_bounds(r.left, val_type, 0, NULL, left, ctx);
         cgen_check_array_bounds(r.right, val_type, 0, NULL, right, ctx);

         LLVMValueRef low = (r.kind == RANGE_TO ? left : right);
         LLVMValueRef ptr = cgen_array_signal_ptr(decl, low);

         type_t type = tree_type(t);
         if (cgen_const_bounds(type))
            return ptr;
         else
            return cgen_array_meta_1(type, left, right, llvm_int8(r.kind), ptr);
      }
      break;

   default:
      assert(false);
   }
}

static void cgen_signal_assign(tree_t t, cgen_ctx_t *ctx)
{
   LLVMValueRef reject = cgen_expr(tree_reject(t), ctx);

   const int nwaveforms = tree_waveforms(t);
   for (int i = 0; i < nwaveforms; i++) {
      tree_t w = tree_waveform(t, i);

      tree_t target = tree_target(t);
      tree_t value  = tree_value(w);

      LLVMValueRef rhs = cgen_expr(value, ctx);
      LLVMValueRef after = (tree_has_delay(w)
                            ? cgen_expr(tree_delay(w), ctx)
                            : llvm_int64(0));

      LLVMValueRef nets = cgen_signal_lvalue(tree_target(t), ctx);

      LLVMValueRef rhs_data, lhs_data, reverse, n_elems, elem_size;

      type_t value_type  = tree_type(value);
      type_t target_type = tree_type(target);

      if (!type_is_array(value_type)) {
         // Need to pass a pointer to values so allocate this on the stack
         LLVMValueRef tmp = LLVMBuildAlloca(builder, llvm_type(value_type), "");
         LLVMBuildStore(builder, rhs, tmp);

         rhs_data  = tmp;
         lhs_data  = nets;
         reverse   = llvm_int1(0);
         n_elems   = llvm_int32(1);
         elem_size = llvm_sizeof(llvm_type(value_type));
      }
      else {
         cgen_check_array_sizes(t, target_type, value_type, nets, rhs, ctx);

         rhs_data = cgen_array_data_ptr(value_type, rhs);
         lhs_data = cgen_array_data_ptr(target_type, nets);

         n_elems   = cgen_array_len_recur(value_type, rhs);
         elem_size = cgen_array_elem_size(value_type);

         LLVMValueRef ldir = cgen_array_dir(target_type, 0, nets);
         LLVMValueRef rdir = cgen_array_dir(value_type, 0, rhs);
         reverse = LLVMBuildICmp(builder, LLVMIntNE, ldir, rdir, "reverse");
      }

      LLVMValueRef args[] = {
         llvm_void_cast(lhs_data),
         llvm_void_cast(rhs_data),
         n_elems,
         elem_size,
         after,
         (i == 0) ? reject : llvm_int64(0),
         reverse
      };
      LLVMBuildCall(builder, llvm_fn("_sched_waveform"),
                    args, ARRAY_LEN(args), "");
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
               cgen_array_left(stype, 0, rval),
               cgen_array_right(stype, 0, rval),
               cgen_array_dir(stype, 0, rval),
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

   const int nassocs = tree_assocs(t);

   LLVMBasicBlockRef else_bb = exit_bb;
   unsigned num_cases = 0;
   for (int i = 0; i < nassocs; i++) {
      if (tree_subkind(tree_assoc(t, i)) == A_OTHERS)
         else_bb = LLVMAppendBasicBlock(ctx->fn, "case_others");
      else
         num_cases++;
   }

   LLVMValueRef val = cgen_expr(tree_value(t), ctx);
   LLVMValueRef sw = LLVMBuildSwitch(builder, val, else_bb, num_cases);

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      switch (tree_subkind(a)) {
      case A_NAMED:
         {
            LLVMBasicBlockRef bb = LLVMAppendBasicBlock(ctx->fn, "");
            LLVMAddCase(sw, cgen_expr(tree_name(a), ctx), bb);

            LLVMPositionBuilderAtEnd(builder, bb);
         }
         break;

      case A_OTHERS:
         LLVMPositionBuilderAtEnd(builder, else_bb);
         break;

      default:
         assert(false);
      }

      cgen_stmt(tree_value(a), ctx);
      LLVMBuildBr(builder, exit_bb);
   }

   LLVMPositionBuilderAtEnd(builder, exit_bb);
}

static void cgen_case_add_branch(case_state_t *where, int left, int right,
                                 int depth, int dirmul,
                                 tree_t value, tree_t stmts)
{
   const int n = left + (depth * dirmul);

   if (((dirmul == -1) && (n < right)) || ((dirmul == 1) && (n > right))) {
      assert(where->stmts == NULL);
      assert(where->narcs == 0);
      where->stmts = stmts;
   }
   else {
      int64_t this;
      bool found = false;
      const int nassocs = tree_assocs(value);
      for (int i = 0; (i < nassocs) && !found; i++) {
         tree_t a = tree_assoc(value, i);
         switch (tree_subkind(a)) {
         case A_NAMED:
            if (assume_int(tree_name(a)) == n) {
               this = assume_int(tree_value(a));
               found = true;
            }
            break;

         case A_POS:
            if (tree_pos(a) == n - MIN(left, right)) {
               this = assume_int(tree_value(a));
               found = true;
            }
            break;

         case A_OTHERS:
            this = assume_int(tree_value(a));
            found = true;
            break;
         }
      }
      assert(found);

      for (int i = 0; i < where->narcs; i++) {
         if (where->arcs[i].value == this) {
            cgen_case_add_branch(where->arcs[i].next,
                                 left, right, depth + 1, dirmul, value, stmts);
            return;
         }
      }

      case_state_t *next = xmalloc(sizeof(case_state_t));
      next->stmts = NULL;
      next->narcs = 0;

      assert(where->narcs < MAX_CASE_ARCS);
      case_arc_t *arc = &(where->arcs[(where->narcs)++]);
      arc->value = this;
      arc->next  = next;

      cgen_case_add_branch(next, left, right, depth + 1,
                           dirmul, value, stmts);
   }
}

static void cgen_case_alloc_basic_blocks(cgen_ctx_t *ctx, case_state_t *state)
{
   state->block = LLVMAppendBasicBlock(ctx->fn, "case_state");
   for (int i = 0; i < state->narcs; i++)
      cgen_case_alloc_basic_blocks(ctx, state->arcs[i].next);
}

static void cgen_case_emit_state_code(cgen_ctx_t *ctx, case_state_t *state,
                                      LLVMValueRef value, int depth,
                                      LLVMBasicBlockRef exit_bb,
                                      LLVMBasicBlockRef others_bb)
{
   LLVMPositionBuilderAtEnd(builder, state->block);

   if (state->stmts != NULL) {
      cgen_stmt(state->stmts, ctx);
      LLVMBuildBr(builder, exit_bb);
   }
   else {
      LLVMValueRef indexes[] = { llvm_int32(depth) };
      LLVMValueRef ptr = LLVMBuildGEP(builder, value,
                                      indexes, ARRAY_LEN(indexes), "");
      LLVMValueRef loaded = LLVMBuildLoad(builder, ptr, "");

      LLVMValueRef sw = LLVMBuildSwitch(builder, loaded,
                                        others_bb, state->narcs);

      for (int i = 0; i < state->narcs; i++) {
         LLVMValueRef cast =
            LLVMBuildIntCast(builder, llvm_int32(state->arcs[i].value),
                             LLVMTypeOf(loaded), "");
         LLVMAddCase(sw, cast, state->arcs[i].next->block);

         cgen_case_emit_state_code(ctx, state->arcs[i].next, value,
                                   depth + 1, exit_bb, others_bb);
      }
   }
}

static void cgen_case_cleanup(case_state_t *state, bool root)
{
   for (int i = 0; i < state->narcs; i++)
      cgen_case_cleanup(state->arcs[i].next, false);
   if (!root)
      free(state);
}

static void cgen_case_array(tree_t t, cgen_ctx_t *ctx)
{
   // Case staments on arrays are implemented by building a decision tree
   // where each state is mapped to an LLVM basic block

   LLVMBasicBlockRef exit_bb   = LLVMAppendBasicBlock(ctx->fn, "case_exit");
   LLVMBasicBlockRef others_bb = exit_bb;

   LLVMValueRef val = cgen_expr(tree_value(t), ctx);
   type_t type = tree_type(tree_value(t));

   const int nassocs = tree_assocs(t);

   case_state_t root = {
      .stmts = NULL,
      .narcs = 0,
      .block = NULL
   };

   range_t r = type_dim(type, 0);
   const int64_t left  = assume_int(r.left);
   const int64_t right = assume_int(r.right);
   const int dirmul = (r.kind == RANGE_DOWNTO) ? -1 : 1;

   int others_assoc = -1;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      switch (tree_subkind(a)) {
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            tree_kind_t kind = tree_kind(name);

            if (kind != T_AGGREGATE) {
               assert(kind == T_REF);
               tree_t decl = tree_ref(name);
               assert(tree_kind(decl) == T_CONST_DECL);
               name = tree_value(decl);
               kind = tree_kind(name);
            }

            assert(kind == T_AGGREGATE);

            cgen_case_add_branch(&root, left, right, 0, dirmul,
                                 name, tree_value(a));
         }
         break;

      case A_OTHERS:
         others_assoc = i;
         others_bb = LLVMAppendBasicBlock(ctx->fn, "case_others");
         break;

      default:
         assert(false);
      }
   }

   LLVMValueRef data_ptr = cgen_array_data_ptr(type, val);

   cgen_case_alloc_basic_blocks(ctx, &root);

   LLVMBuildBr(builder, root.block);

   cgen_case_emit_state_code(ctx, &root, data_ptr, 0, exit_bb, others_bb);
   cgen_case_cleanup(&root, true);

   if (others_assoc != -1) {
      LLVMPositionBuilderAtEnd(builder, others_bb);
      cgen_stmt(tree_value(tree_assoc(t, others_assoc)), ctx);
      LLVMBuildBr(builder, exit_bb);
   }

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
   const int cover_tag = tree_attr_int(t, stmt_tag_i, -1);
   if (cover_tag != -1) {
      LLVMValueRef cover_counts = LLVMGetNamedGlobal(module, "cover_stmts");

      LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(cover_tag) };
      LLVMValueRef count_ptr = LLVMBuildGEP(builder, cover_counts,
                                            indexes, ARRAY_LEN(indexes), "");

      LLVMValueRef count = LLVMBuildLoad(builder, count_ptr, "cover_count");
      LLVMValueRef count1 = LLVMBuildAdd(builder, count, llvm_int32(1), "");

      LLVMBuildStore(builder, count1, count_ptr);
   }

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

static LLVMValueRef cgen_resolution_func(type_t type)
{
   // Resolution functions are in LRM 93 section 2.4

   while (type_is_array(type)
          && ((type_kind(type) != T_SUBTYPE) || !type_has_resolution(type)))
      type = type_elem(type);

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

   const int nnets = tree_nets(t);
   assert(nnets > 0);

   char buf[256];
   snprintf(buf, sizeof(buf), "%s_nets", istr(tree_ident(t)));

   LLVMTypeRef  nid_type = cgen_net_id_type();
   LLVMTypeRef  map_type = LLVMArrayType(nid_type, nnets);
   LLVMValueRef map_var  = LLVMAddGlobal(module, map_type, buf);
   LLVMSetLinkage(map_var, LLVMInternalLinkage);

   if (nnets <= MAX_STATIC_NETS) {
      // Generate a constant mapping table from sub-element to net ID
      LLVMSetGlobalConstant(map_var, true);

      LLVMValueRef init[nnets];
      for (int i = 0; i < nnets; i++)
         init[i] = llvm_int32(tree_net(t, i));

      LLVMSetInitializer(map_var, LLVMConstArray(nid_type, init, nnets));
   }
   else {
      // Values will be filled in by reset function
      LLVMSetInitializer(map_var, LLVMGetUndef(map_type));
   }

   tree_add_attr_ptr(t, sig_nets_i, map_var);
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
                                 cgen_array_left(value_type, 0, var),
                                 cgen_array_right(value_type, 0, var),
                                 cgen_array_dir(value_type, 0, var),
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
         tree_add_attr_ptr(p, sig_nets_i, LLVMGetParam(fn, i));
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
         tree_add_attr_ptr(p, sig_nets_i, LLVMGetParam(fn, i));
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
      case C_FILE:
         tree_add_attr_ptr(p, local_var_i, LLVMGetParam(fn, i));
         break;

      default:
         assert(false);
      }
   }

   struct cgen_ctx ctx = {
      .entry_list = NULL,
      .proc       = NULL,
      .fdecl      = t,
      .fn         = fn,
      .state      = NULL
   };

   tree_visit_only(t, cgen_func_constants, &ctx, T_CONST_DECL);

   // Generate a jump table to handle resuming from a wait statement

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

static void cgen_net_mapping_table(tree_t d, int offset, netid_t first,
                                   netid_t last, cgen_ctx_t *ctx)
{
   LLVMValueRef nets = cgen_signal_nets(d);

   LLVMValueRef i = LLVMBuildAlloca(builder, cgen_net_id_type(), "i");
   LLVMBuildStore(builder, llvm_int32(first), i);

   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "nm_test");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "nm_body");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "nm_exit");

   LLVMBuildBr(builder, test_bb);

   // Loop test

   LLVMPositionBuilderAtEnd(builder, test_bb);

   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i");
   LLVMValueRef done = LLVMBuildICmp(builder, LLVMIntUGT, i_loaded,
                                     llvm_int32(last), "done");
   LLVMBuildCondBr(builder, done, exit_bb, body_bb);

   // Loop body

   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef indexes[] = {
      llvm_int32(0),
      LLVMBuildAdd(builder,
                   LLVMBuildSub(builder, i_loaded, llvm_int32(first), ""),
                   llvm_int32(offset), "")
   };
   LLVMValueRef ptr = LLVMBuildGEP(builder, nets,
                                   indexes, ARRAY_LEN(indexes), "ptr");
   LLVMBuildStore(builder, i_loaded, ptr);

   LLVMValueRef i_plus_1 = LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "");
   LLVMBuildStore(builder, i_plus_1, i);

   LLVMBuildBr(builder, test_bb);

   // Epilogue

   LLVMPositionBuilderAtEnd(builder, exit_bb);
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
         tree_t value = tree_value(d);
         LLVMValueRef init = cgen_expr(value, &ctx);
         type_t type = tree_type(d);
         if (type_is_array(type) && cgen_const_bounds(type))
            cgen_array_copy(tree_type(value), type, init,
                            (LLVMValueRef)global, NULL, &ctx);
         else
            LLVMBuildStore(builder, init, (LLVMValueRef)global);
      }

      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;

      const int nnets = tree_nets(d);
      if (nnets > MAX_STATIC_NETS) {
         // Need to generate runtime code to fill in net mapping table

         netid_t first = tree_net(d, 0);
         int     off   = 0;
         netid_t last  = first;
         for (int i = 1; i < nnets; i++) {
            const netid_t this = tree_net(d, i);
            if (this != last + 1) {
               cgen_net_mapping_table(d, off, first, last, &ctx);
               first = last = this;
               off = i;
            }
            else
               last = this;
         }
         cgen_net_mapping_table(d, off, first, last, &ctx);
      }

      // Internal signals that were generated from ports will not have
      // an initial value
      if (!tree_has_value(d))
         continue;

      // Set the initial value of the net
      LLVMValueRef val = cgen_expr(tree_value(d), &ctx);

      type_t type = tree_type(d);

      LLVMValueRef n_elems, size;
      if (!type_is_array(type)) {
         // Need to get a pointer to the data
         LLVMTypeRef lltype = LLVMTypeOf(val);
         LLVMValueRef tmp = LLVMBuildAlloca(builder, lltype, "");
         LLVMBuildStore(builder, val, tmp);
         val = tmp;

         n_elems = llvm_int32(1);
         size    = llvm_sizeof(lltype);
      }
      else {
         n_elems = cgen_array_len_recur(type, val);
         size    = cgen_array_elem_size(type);
      }

      // Assuming array nets are sequential
      netid_t nid = tree_net(d, 0);

      LLVMValueRef args[] = {
         llvm_int32(nid),
         llvm_void_cast(val),
         n_elems,
         size,
         llvm_void_cast(cgen_resolution_func(type)),
         llvm_int32(tree_index(d)),
         LLVMBuildPointerCast(builder, mod_name,
                              LLVMPointerType(LLVMInt8Type(), 0), "")
      };
      LLVMBuildCall(builder, llvm_fn("_set_initial"),
                    args, ARRAY_LEN(args), "");
   }

   LLVMValueRef cover_stmts = LLVMGetNamedGlobal(module, "cover_stmts");
   if (cover_stmts != NULL) {
      LLVMValueRef memset_args[] = {
         llvm_void_cast(cover_stmts),
         llvm_int8(0),
         llvm_int32(tree_attr_int(t, ident_new("stmt_tags"), 0) * 4),
         llvm_int32(4),
         llvm_int1(false)
      };

      LLVMBuildCall(builder, llvm_fn("llvm.memset.p0i8.i32"),
                    memset_args, ARRAY_LEN(memset_args), "");
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

static void cgen_coverage_state(tree_t t)
{
   int stmt_tags = tree_attr_int(t, ident_new("stmt_tags"), 0);
   if (stmt_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_stmts");
      LLVMSetInitializer(var, LLVMGetUndef(type));
   }
}

static void cgen_top(tree_t t)
{
   cgen_coverage_state(t);

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
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt64Type(),
      LLVMInt64Type(),
      LLVMInt1Type()
   };
   LLVMAddFunction(module, "_sched_waveform",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_waveform_args,
                                    ARRAY_LEN(_sched_waveform_args),
                                    false));

   LLVMTypeRef _sched_event_args[] = {
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt1Type()
   };
   LLVMAddFunction(module, "_sched_event",
                   LLVMFunctionType(LLVMVoidType(),
                                    _sched_event_args,
                                    ARRAY_LEN(_sched_event_args),
                                    false));

   LLVMTypeRef _set_initial_args[] = {
      LLVMInt32Type(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0)
   };
   LLVMAddFunction(module, "_set_initial",
                   LLVMFunctionType(LLVMVoidType(),
                                    _set_initial_args,
                                    ARRAY_LEN(_set_initial_args),
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

   LLVMTypeRef llvm_memset_args[] = {
      LLVMPointerType(LLVMInt8Type(), 0),
      LLVMInt8Type(),
      LLVMInt32Type(),
      LLVMInt32Type(),
      LLVMInt1Type()
   };
   LLVMAddFunction(module, "llvm.memset.p0i8.i32",
                   LLVMFunctionType(LLVMVoidType(),
                                    llvm_memset_args,
                                    ARRAY_LEN(llvm_memset_args),
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

   LLVMTypeRef _div_zero_args[] = {
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0)
   };
   LLVMAddFunction(module, "_div_zero",
                   LLVMFunctionType(LLVMVoidType(),
                                    _div_zero_args,
                                    ARRAY_LEN(_div_zero_args),
                                    false));

   LLVMTypeRef _null_deref_args[] = {
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt8Type(), 0)
   };
   LLVMAddFunction(module, "_null_deref",
                   LLVMFunctionType(LLVMVoidType(),
                                    _null_deref_args,
                                    ARRAY_LEN(_null_deref_args),
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

   LLVMTypeRef _bit_shift_args[] = {
      LLVMInt32Type(),
      LLVMPointerType(LLVMInt1Type(), 0),
      LLVMInt32Type(),
      LLVMInt8Type(),
      LLVMInt32Type(),
      LLVMPointerType(llvm_uarray_type(LLVMInt1Type(), 1), 0)
   };
   LLVMAddFunction(module, "_bit_shift",
                   LLVMFunctionType(LLVMVoidType(),
                                    _bit_shift_args,
                                    ARRAY_LEN(_bit_shift_args),
                                    false));

   LLVMTypeRef _test_net_flag_args[] = {
      llvm_void_ptr(),
      LLVMInt32Type(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_test_net_flag",
                   LLVMFunctionType(LLVMInt1Type(),
                                    _test_net_flag_args,
                                    ARRAY_LEN(_test_net_flag_args),
                                    false));

   LLVMTypeRef _last_event_args[] = {
      llvm_void_ptr(),
      LLVMInt32Type()
   };
   LLVMAddFunction(module, "_last_event",
                   LLVMFunctionType(LLVMInt64Type(),
                                    _last_event_args,
                                    ARRAY_LEN(_last_event_args),
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
   sig_nets_i     = ident_new("sig_nets");
   foreign_i      = ident_new("FOREIGN");
   never_waits_i  = ident_new("never_waits");
   stmt_tag_i     = ident_new("stmt_tag");

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
