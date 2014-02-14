//
//  Copyright (C) 2011-2014  Nick Gasson
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

static ident_t var_offset_i;
static ident_t local_var_i;
static ident_t global_const_i;
static ident_t sig_nets_i;
static ident_t foreign_i;
static ident_t never_waits_i;
static ident_t stmt_tag_i;
static ident_t cond_tag_i;
static ident_t sub_cond_i;
static ident_t elide_bounds_i;
static ident_t null_range_i;
static ident_t nest_level_i;
static ident_t nest_offset_i;
static ident_t nest_parent_i;
static ident_t returned_i;
static ident_t static_i;

typedef struct case_arc   case_arc_t;
typedef struct case_state case_state_t;
typedef struct proc_entry proc_entry_t;
typedef struct block_list block_list_t;

// Linked list of entry points to a process
// These correspond to wait statements
struct proc_entry {
   int               state_num;
   tree_t            wait;
   LLVMBasicBlockRef bb;
   proc_entry_t     *next;
};

// Linked list of named blocks such as loops
struct block_list {
   ident_t            name;
   LLVMBasicBlockRef  exit_bb;
   LLVMBasicBlockRef  entry_bb;
   block_list_t      *next;
};

// Code generation context for a process or function
typedef struct cgen_ctx {
   proc_entry_t  *entry_list;
   block_list_t  *blocks;
   LLVMValueRef   state;
   LLVMValueRef   nest_state;
   LLVMValueRef   fn;
   tree_t         proc;
   tree_t         fdecl;
   bool           tmp_stack_used;
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
static LLVMValueRef cgen_const_record(tree_t t, bool nest, cgen_ctx_t *ctx);
static int cgen_array_dims(type_t type);
static LLVMValueRef cgen_signal_nets(tree_t decl);
static void cgen_check_bounds(tree_t t, LLVMValueRef kind, LLVMValueRef value,
                              LLVMValueRef min, LLVMValueRef max,
                              cgen_ctx_t *ctx);
static LLVMValueRef cgen_support_fn(const char *name);
static LLVMValueRef cgen_signal_lvalue(tree_t t, cgen_ctx_t *ctx);
static void cgen_proc_body(tree_t t, tree_t parent);
static void cgen_func_body(tree_t t, tree_t parent);
static void cgen_cond_coverage(tree_t t, LLVMValueRef value);
static LLVMValueRef cgen_array_rel(LLVMValueRef lhs, LLVMValueRef rhs,
                                   type_t left_type, type_t right_type,
                                   LLVMIntPredicate pred, cgen_ctx_t *ctx);
static LLVMValueRef cgen_tmp_alloc(LLVMValueRef bytes, LLVMTypeRef type);
static LLVMValueRef cgen_dyn_aggregate(tree_t t, bool use_tmp, cgen_ctx_t *ctx);

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
   if ((fn == NULL) && ((fn = cgen_support_fn(name)) == NULL))
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
         uint64_t elements = assume_int(r.right) - assume_int(r.left) + 1;

         if (elements <= 2)
            return 1;
         if (elements <= UINT64_C(0x100))
            return 8;
         else if (elements <= UINT64_C(0x10000))
            return 16;
         else if (elements <= UINT64_C(0x100000000))
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

   case T_RECORD:
      {
         unsigned width = 0;

         const int nfields = type_fields(t);
         for (int i = 0; i < nfields; i++)
            width += bit_width(tree_type(type_field(t, i)));

         return width;
      }

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
            const int ndims = type_dims(t);
            for (int i = 0; i < ndims; i++) {
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
      // Mangle the VHDL name into a unique symbol name

      const char *name = istr(tree_ident(decl));
      char tmp[strlen(name) + 1], *p;
      for (p = tmp; *name != '\0'; ++name) {
         switch (*name) {
         case '"': break;
         case '+': *p++ = 'p'; break;
         case '-': *p++ = 's'; break;
         case '*': *p++ = 'm'; break;
         case '/': *p++ = 'd'; break;
         case '=': *p++ = 'e'; break;
         case '>': *p++ = 'g'; break;
         case '<': *p++ = 'l'; break;
         default: *p++ = *name;
         }
      }
      *p = '\0';

      static_printf(buf, "%s", tmp);

      const int nparams = type_params(type);
      for (int i = 0; i < nparams; i++) {
         type_t param = type_param(type, i);
         static_printf(buf, "$%s", istr(type_ident(param)));
      }

      if (type_kind(type) == T_FUNC)
         static_printf(buf, "_return_%s", istr(type_ident(type_result(type))));
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
   if (type_is_unconstrained(type))
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
   if (type_is_unconstrained(type))
      return type_index_constrs(type);
   else
      return type_dims(type);
}

static LLVMTypeRef cgen_net_id_type(void)
{
   return LLVMInt32Type();
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
      if (var == NULL) {
         assert(!type_is_unconstrained(type));
         range_t r = type_dim(type, dim);
         if ((r.kind == RANGE_TO) || (r.kind == RANGE_DOWNTO))
            return llvm_int8(r.kind);
         else {
            if (r.kind == RANGE_DYN) {
               // This can only appear when using 'RANGE
               assert(tree_kind(r.left) == T_FCALL);
               tree_t p = tree_param(r.left, 1);
               tree_t value = tree_value(p);
               assert(tree_kind(value) == T_REF);

               LLVMValueRef uarray;
               tree_t decl = tree_ref(value);
               if (cgen_get_class(decl) == C_SIGNAL)
                  uarray = cgen_signal_nets(decl);
               else
                  uarray = cgen_get_var(decl, NULL);

               return cgen_array_dir(type, dim, uarray);
            }
            else if (r.kind == RANGE_RDYN) {
               // TODO: 'REVERSE_RANGE
               assert(false);
            }
            else
               assert(false);
         }
      }
      else {
         LLVMValueRef ldim = cgen_uarray_dim(var, dim);
         return LLVMBuildExtractValue(builder, ldim, 2, "dir");
      }
   }
   else
      return llvm_int8(type_dim(type, dim).kind);
}

static LLVMValueRef cgen_array_left(type_t type, int dim, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      if (var == NULL) {
         assert(!type_is_unconstrained(type));
         return cgen_expr(type_dim(type, dim).left, NULL);
      }
      else {
         LLVMValueRef ldim = cgen_uarray_dim(var, dim);
         return LLVMBuildExtractValue(builder, ldim, 0, "left");
      }
   }
   else
      return llvm_int32(assume_int(type_dim(type, dim).left));
}

static LLVMValueRef cgen_array_right(type_t type, int dim, LLVMValueRef var)
{
   if (!cgen_const_bounds(type)) {
      if (var == NULL) {
         assert(!type_is_unconstrained(type));
         return cgen_expr(type_dim(type, dim).right, NULL);
      }
      else {
         LLVMValueRef ldim = cgen_uarray_dim(var, dim);
         return LLVMBuildExtractValue(builder, ldim, 1, "right");
      }
   }
   else
      return llvm_int32(assume_int(type_dim(type, dim).right));
}

static int cgen_const_array_len(type_t type, int dim)
{
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

   return n_elems;
}

static LLVMValueRef cgen_array_len(type_t type, int dim, LLVMValueRef data)
{
   // Passing dimension -1 means sum over all dimensions

   if (cgen_const_bounds(type))
      return llvm_int32(cgen_const_array_len(type, dim));
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

static LLVMValueRef cgen_tmp_var(type_t type, const char *name,
                                 bool use_tmp, cgen_ctx_t *ctx)
{
   // Handle case where array size is not known until run time
   if (type_is_array(type) && !cgen_const_bounds(type)) {
      const int dims = cgen_array_dims(type);
      LLVMValueRef params[dims][3];

      LLVMValueRef size = llvm_int32(1);

      for (int i = 0; i < dims; i++) {
         range_t r = type_dim(type, i);

         LLVMValueRef kind_ll = cgen_array_dir(type, i, NULL);
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

      LLVMValueRef ptr;
      if (use_tmp) {
         LLVMValueRef bytes =
            LLVMBuildMul(builder, llvm_sizeof(base_type), size, "bytes");
         ptr = cgen_tmp_alloc(bytes, base_type);
      }
      else {
         LLVMValueRef buf =
            LLVMBuildArrayAlloca(builder, base_type, size, "buf");
         ptr = LLVMBuildPointerCast(builder, buf, ptr_type, "");
      }

      LLVMValueRef meta = cgen_array_meta(type, params, ptr);
      LLVMSetValueName(meta, name);
      return meta;
   }
   else if (use_tmp && type_is_array(type)) {
      LLVMValueRef bytes = llvm_sizeof(llvm_type(type));
      return cgen_tmp_alloc(bytes, llvm_type(type_elem(type)));
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

   LLVMValueRef zeroed;
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

      LLVMValueRef downto_zero = LLVMBuildSub(builder, left, off, "");
      LLVMValueRef to_zero     = LLVMBuildSub(builder, off, left, "");

      zeroed = LLVMBuildSelect(builder, is_downto, downto_zero, to_zero, "");
   }
   else {
      range_t r = type_dim(type, dim);
      LLVMValueRef left = cgen_expr(r.left, ctx);
      if (r.kind == RANGE_TO)
         zeroed = LLVMBuildSub(builder, off, left, "");
      else
         zeroed = LLVMBuildSub(builder, left, off, "");
   }

   // Array offsets are always 32-bit
   return LLVMBuildZExt(builder, zeroed, LLVMInt32Type(), "");
}

static void cgen_check_bounds_hint(tree_t t, tree_t hint, LLVMValueRef kind,
                                   LLVMValueRef value, LLVMValueRef min,
                                   LLVMValueRef max, cgen_ctx_t *ctx)
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

   LLVMValueRef index = llvm_int32(tree_index(t));

   LLVMValueRef args[] = {
      index,
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      value,
      min,
      max,
      kind,
      (t == hint) ? index : llvm_int32(tree_index(hint))
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static void cgen_check_bounds(tree_t t, LLVMValueRef kind, LLVMValueRef value,
                              LLVMValueRef min, LLVMValueRef max,
                              cgen_ctx_t *ctx)
{
   cgen_check_bounds_hint(t, t, kind, value, min, max, ctx);
}

static void cgen_check_scalar_bounds_hint(tree_t t, tree_t hint,
                                          LLVMValueRef value, cgen_ctx_t *ctx)
{
   type_t type = tree_type(t);

   if (type_is_enum(type)) {
      const int max = type_enum_literals(type_base_recur(type)) - 1;
      cgen_check_bounds_hint(t, hint,llvm_int32(BOUNDS_ENUM), value,
                             llvm_int32(0), llvm_int32(max), ctx);
   }
   else if (type_is_integer(type)) {
      range_t r = type_dim(type, 0);
      LLVMValueRef min =
         cgen_expr((r.kind == RANGE_TO) ? r.left : r.right, ctx);
      LLVMValueRef max =
         cgen_expr((r.kind == RANGE_TO) ? r.right : r.left, ctx);
      LLVMValueRef kind = llvm_int32((r.kind == RANGE_TO)
                                     ? BOUNDS_TYPE_TO : BOUNDS_TYPE_DOWNTO);
      cgen_check_bounds_hint(t, hint, kind, value, min, max, ctx);
   }
   else if (type_is_real(type)) {
      // TODO
   }
}

static void cgen_check_scalar_bounds(tree_t t, LLVMValueRef value,
                                     cgen_ctx_t *ctx)
{
   cgen_check_scalar_bounds_hint(t, t, value, ctx);
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

static void cgen_check_array_sizes(tree_t t, type_t ltype,
                                   type_t rtype, LLVMValueRef lval,
                                   LLVMValueRef rval, cgen_ctx_t *ctx)
{
   LLVMValueRef llen = cgen_array_len(ltype, 0, lval);
   LLVMValueRef rlen = cgen_array_len(rtype, 0, rval);

   LLVMValueRef ok = LLVMBuildICmp(builder, LLVMIntEQ, llen, rlen, "ok");

   LLVMBasicBlockRef pass_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_pass");
   LLVMBasicBlockRef fail_bb  = LLVMAppendBasicBlock(ctx->fn, "bounds_fail");

   LLVMBuildCondBr(builder, ok, pass_bb, fail_bb);

   LLVMPositionBuilderAtEnd(builder, fail_bb);

   LLVMValueRef index = llvm_int32(tree_index(t));

   LLVMValueRef args[] = {
      index,
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), ""),
      llvm_int32(0),
      llen,
      rlen,
      llvm_int32(BOUNDS_ARRAY_SIZE),
      index
   };

   LLVMBuildCall(builder, llvm_fn("_bounds_fail"), args, ARRAY_LEN(args), "");

   LLVMBuildUnreachable(builder);

   LLVMPositionBuilderAtEnd(builder, pass_bb);
}

static LLVMValueRef cgen_unalias_index(tree_t alias, LLVMValueRef index,
                                       LLVMValueRef meta, cgen_ctx_t *ctx)
{
   type_t alias_type = tree_type(alias);
   type_t base_type  = tree_type(tree_value(alias));

   assert(type_dims(alias_type) == 1);  // TODO: multi-dimensional arrays

   range_t alias_r = type_dim(alias_type, 0);
   LLVMValueRef off = LLVMBuildSub(builder, index,
                                   cgen_expr(alias_r.left, ctx), "");

   LLVMValueRef bleft, bdir;
   switch (type_kind(base_type)) {
   case T_CARRAY:
   case T_SUBTYPE:
      // The transformation is a constant offset of indices
      {
         range_t base_r = type_dim(base_type, 0);
         bleft = cgen_expr(base_r.left, ctx);
         bdir  = cgen_array_dir(base_type, 0, NULL);
      }
      break;

   case T_UARRAY:
      // The transformation must be computed at runtime
      {
         bleft = cgen_array_left(base_type, 0, meta);
         bdir  = cgen_array_dir(base_type, 0, meta);
      }
      break;

   default:
      assert(false);
   }

   LLVMValueRef adir = cgen_array_dir(alias_type, 0, NULL);

   LLVMValueRef same_dir =
      LLVMBuildICmp(builder, LLVMIntEQ, bdir, adir, "same_dir");

   return LLVMBuildSelect(
      builder,
      same_dir,
      LLVMBuildAdd(builder, bleft, off, ""),
      LLVMBuildSub(builder, bleft, off, ""),
      "unalias");
}

static LLVMValueRef cgen_get_slice(LLVMValueRef array, type_t type,
                                   tree_t alias, range_t r, cgen_ctx_t *ctx)
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

   LLVMValueRef data = cgen_array_data_ptr(type, array);

   LLVMValueRef kind;
   if (alias != NULL) {
      tree_t aliased = tree_value(alias);
      array = cgen_expr(aliased, ctx);
      left  = cgen_unalias_index(alias, left, array, ctx);
      right = cgen_unalias_index(alias, right, array, ctx);
      type  = tree_type(aliased);
      kind  = cgen_array_dir(type, 0, array);
   }
   else
      kind = llvm_int8(r.kind);

   LLVMValueRef off = cgen_array_off(left, array, type, ctx, 0);

   LLVMTypeRef ptr_type = LLVMPointerType(llvm_type(type_elem(type)), 0);

   LLVMValueRef ptr_norm = LLVMBuildGEP(builder, data, &off, 1, "");
   LLVMValueRef ptr_null = LLVMConstNull(ptr_type);

   LLVMValueRef ptr = LLVMBuildSelect(builder, null, ptr_null, ptr_norm, "ptr");

   bool unwrap = cgen_is_const(r.left) && cgen_is_const(r.right);

   if (unwrap)
      return ptr;
   else
      return cgen_array_meta_1(type, left, right, kind, ptr);
}

static LLVMValueRef cgen_array_signal_ptr(tree_t decl, LLVMValueRef elem,
                                          cgen_ctx_t *ctx)
{
   // If we see a port declaration here outside a subprogram
   // then it is OPEN
   const tree_kind_t decl_kind = tree_kind(decl);
   if ((decl_kind == T_PORT_DECL) && (ctx->fdecl == NULL))
      return NULL;
   else if ((decl_kind != T_PORT_DECL) && (decl_kind != T_SIGNAL_DECL))
      return NULL;

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
      LLVMValueRef indexes[] = { elem };
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
      if ((type_is_array(type) && !cgen_const_bounds(type)))
         return LLVMBuildLoad(builder, (LLVMValueRef)global, "global");
      else
         return global;
   }

   tree_kind_t kind = tree_kind(decl);
   assert((kind == T_VAR_DECL)
          || (kind == T_CONST_DECL)
          || (kind == T_FILE_DECL)
          || (kind == T_PORT_DECL));

   type_t type = tree_type(decl);

   const int offset = tree_attr_int(decl, var_offset_i, -1);
   if (offset != -1) {
      assert(ctx != NULL);
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

   const int nest = tree_attr_int(decl, nest_offset_i, -1);
   if (nest != -1) {
      // This variable is contained in an outer scope

      const int our_level =
         (ctx->fdecl ? tree_attr_int(ctx->fdecl, nest_level_i, 0) : 0);
      const int var_level = tree_attr_int(decl, nest_level_i, 0);

      // Walk through the chain of structures to get the correct level
      LLVMValueRef state = ctx->nest_state;
      for (int i = our_level; i > var_level; i--)
         state = LLVMBuildExtractValue(builder, state, 0, "up");

      return LLVMBuildExtractValue(builder, state, nest, "");
   }

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

static const char *cgen_memcpy_name(int width)
{
   static char name[64];
   checked_sprintf(name, sizeof(name),
                   "llvm.memcpy.p0i%d.p0i%d.i32", width, width);
   return name;
}

static void cgen_record_copy(type_t type, LLVMValueRef src, LLVMValueRef dst)
{
   LLVMValueRef memcpy_args[] = {
      llvm_void_cast(dst),
      llvm_void_cast(src),
      llvm_sizeof(llvm_type(type)),
      llvm_int32(4),
      llvm_int1(0)
   };
   LLVMBuildCall(builder, llvm_fn(cgen_memcpy_name(8)),
                 memcpy_args, ARRAY_LEN(memcpy_args), "");
}

static void cgen_array_copy(type_t src_type, type_t dest_type,
                            LLVMValueRef src, LLVMValueRef dst,
                            LLVMValueRef offset, cgen_ctx_t *ctx)
{
   LLVMValueRef ll_n_elems = cgen_array_len_recur(src_type, src);

   if (!cgen_const_bounds(dest_type))
      dst = cgen_array_data_ptr(dest_type, dst);

   LLVMValueRef src_ptr = cgen_array_data_ptr(src_type, src);

   if (offset == NULL)
      offset = llvm_int32(0);

   LLVMValueRef indexes[] = { offset };
   LLVMValueRef dst_ptr = LLVMBuildGEP(builder, dst, indexes,
                                       ARRAY_LEN(indexes), "dst_ptr");

   type_t elem_type = type_elem(src_type);

   int align, width;
   LLVMValueRef bytes;
   if (type_is_record(elem_type)) {
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
   LLVMBuildCall(builder, llvm_fn(cgen_memcpy_name(width)),
                 memcpy_args, ARRAY_LEN(memcpy_args), "");
}

static LLVMValueRef cgen_local_var(tree_t d, cgen_ctx_t *ctx)
{
   type_t type = tree_type(d);
   const bool use_tmp = tree_attr_int(d, returned_i, 0);

   if (tree_has_value(d)) {
      tree_t value = tree_value(d);

      const bool dyn_agg =
         type_is_array(type)
         && (tree_kind(value) == T_AGGREGATE)
         && (!cgen_const_bounds(tree_type(value)) || !cgen_is_const(value));

      if (dyn_agg) {
         LLVMValueRef var = cgen_dyn_aggregate(value, use_tmp, ctx);
         LLVMSetValueName(var, istr(tree_ident(d)));
         return var;
      }
      else {
         LLVMValueRef var =
            cgen_tmp_var(type, istr(tree_ident(d)), use_tmp, ctx);
         LLVMValueRef init = cgen_expr(value, ctx);

         if (type_is_array(type)) {
            type_t value_type = tree_type(value);
            cgen_check_array_sizes(d, type, value_type, var, init, ctx);
            cgen_array_copy(value_type, type, init, var, NULL, ctx);
         }
         else if (type_is_record(type))
            cgen_record_copy(type, init, var);
         else {
            cgen_check_scalar_bounds(d, init, ctx);
            LLVMBuildStore(builder, init, var);
         }

         return var;
      }
   }
   else
      return cgen_tmp_var(type, istr(tree_ident(d)), use_tmp, ctx);
}

static LLVMTypeRef cgen_nest_struct_type(tree_t parent)
{
   tree_t grandparent = tree_attr_tree(parent, nest_parent_i);

   const bool has_ports = (tree_kind(parent) != T_PROCESS);

   const int nports  = has_ports ? tree_ports(parent) : 0;
   const int ndecls  = tree_decls(parent);
   const int nfields = ndecls + nports + (grandparent ? 1 : 0);

   LLVMTypeRef *fields = xmalloc(nfields * sizeof(LLVMTypeRef));

   unsigned offset = 0;

   if (grandparent != NULL)
      fields[offset++] = cgen_nest_struct_type(grandparent);

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(parent, i);
      type_t type = tree_type(p);
      if (type_is_array(type) && cgen_const_bounds(type))
         fields[offset++] = LLVMPointerType(llvm_type(type_elem(type)), 0);
      else
         fields[offset++] = llvm_type(tree_type(p));
   }

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(parent, i);
      switch (tree_kind(d)) {
      case T_CONST_DECL:
      case T_VAR_DECL:
         {
            type_t type = tree_type(d);
            LLVMTypeRef lt;
            if (type_is_array(type) && cgen_const_bounds(type))
               lt = LLVMPointerType(llvm_type(type_elem(type)), 0);
            else
               lt = LLVMPointerType(llvm_type(type), 0);
            fields[offset++] = lt;
         }
         break;
      default:
         break;
      }
   }

   LLVMTypeRef st = LLVMStructType(fields, offset, false);
   free(fields);
   return st;
}

static void cgen_prototype(tree_t t, LLVMTypeRef *args,
                           unsigned *nargs, bool procedure, tree_t parent)
{
   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      type_t type = tree_type(p);

      port_mode_t mode = tree_subkind(p);
      const bool array = type_is_array(type);

      switch (tree_class(p)) {
      case C_SIGNAL:
         {
            if (type_is_array(type)) {
               if (!cgen_const_bounds(type))
                  args[i] = llvm_uarray_type(cgen_net_id_type(),
                                             cgen_array_dims(type));
               else
                  args[i] = LLVMPointerType(cgen_net_id_type(), 0);
            }
            else
               args[i] = LLVMPointerType(cgen_net_id_type(), 0);
         }
         break;

      case C_VARIABLE:
      case C_DEFAULT:
      case C_CONSTANT:
         {
            const bool need_ptr = (((mode == PORT_OUT)
                                    || (mode == PORT_INOUT)
                                    || type_is_record(type))
                                   && !array);

            if (need_ptr || (array && (type_kind(type) != T_UARRAY)))
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

   *nargs = nports;

   if (parent != NULL)
      args[(*nargs)++] = cgen_nest_struct_type(parent);

   if (procedure) {
      // Last parameter is context
      args[(*nargs)++] = llvm_void_ptr();
   }
}

static LLVMValueRef cgen_fdecl(tree_t t, tree_t parent)
{
   const char *mangled = cgen_mangle_func_name(t);
   LLVMValueRef fn = LLVMGetNamedFunction(module, mangled);
   if (fn != NULL)
      return fn;
   else {
      type_t ftype = tree_type(t);

      unsigned nargs;
      LLVMTypeRef atypes[tree_ports(t) + 1];
      cgen_prototype(t, atypes, &nargs, false, parent);

      type_t rtype = type_result(ftype);
      LLVMTypeRef llrtype;
      if (type_is_array(rtype) && cgen_const_bounds(rtype))
         llrtype = LLVMPointerType(llvm_type(type_elem(rtype)), 0);
      else if (type_is_record(rtype))
         llrtype = LLVMPointerType(llvm_type(rtype), 0);
      else
         llrtype = llvm_type(rtype);

      return LLVMAddFunction(
         module,
         mangled,
         LLVMFunctionType(llrtype, atypes, nargs, false));
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
      unsigned nargs;
      LLVMTypeRef atypes[nports + 1];
      cgen_prototype(t, atypes, &nargs, true, NULL);

      return LLVMAddFunction(
         module,
         cgen_mangle_func_name(t),
         LLVMFunctionType(llvm_void_ptr(),
                          atypes,
                          nargs,
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
   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      LLVMConstNull(llvm_void_ptr()),
      llvm_int32(0),
      llvm_int32(0),
      llvm_int1(last_value)
   };
   LLVMValueRef r = LLVMBuildCall(builder, llvm_fn("_vec_load"),
                                  args, ARRAY_LEN(args), "");
   LLVMTypeRef ptr_type = LLVMPointerType(llvm_type(type), 0);
   LLVMValueRef loaded = LLVMBuildPointerCast(builder, r, ptr_type, "");

   return LLVMBuildLoad(builder, loaded, "");
}

static LLVMValueRef cgen_vec_load(LLVMValueRef nets, type_t type,
                                  type_t slice_type, bool last_value,
                                  cgen_ctx_t *ctx)
{
   // Copy the resolved signal into a temporary array

   const bool src_uarray = !cgen_const_bounds(type);
   const bool dst_uarray = !cgen_const_bounds(slice_type);

   LLVMValueRef fn = llvm_fn("_vec_load");

   LLVMValueRef left, right, dir;
   if (type_is_unconstrained(slice_type)) {
      left  = cgen_array_left(slice_type, 0, nets);
      right = cgen_array_right(slice_type, 0, nets);
      dir   = cgen_array_dir(slice_type, 0, nets);
   }
   else {
      range_t r = type_dim(slice_type, 0);
      left  = cgen_expr(r.left, ctx);
      right = cgen_expr(r.right, ctx);
      dir   = llvm_int8(r.kind);
   }

   LLVMValueRef low_abs  = cgen_array_off(left, nets, type, ctx, 0);
   LLVMValueRef high_abs = cgen_array_off(right, nets, type, ctx, 0);

   LLVMValueRef tmp;
   if (dst_uarray) {
      LLVMValueRef length =
         LLVMBuildAdd(builder,
                      LLVMBuildSub(builder, high_abs, low_abs, ""),
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

   LLVMValueRef args[] = {
      llvm_void_cast(p_signal),
      llvm_void_cast(tmp),
      low_abs,
      high_abs,
      llvm_int1(last_value)
   };
   LLVMValueRef r = LLVMBuildCall(builder, fn, args, ARRAY_LEN(args), "");

   // If the signal data is contiguous _vec_load will return a pointer to
   // the internal representation, otherwise it will copy into the tmp buffer
   // and return a pointer to that
   LLVMValueRef loaded = LLVMBuildPointerCast(builder, r, LLVMTypeOf(tmp), "");

   if (dst_uarray)
      return cgen_array_meta_1(slice_type, left, right, dir, loaded);
   else
      return loaded;
}

static LLVMValueRef cgen_signal_nets(tree_t decl)
{
   // Return the array of nets associated with a signal
   LLVMValueRef nets = tree_attr_ptr(decl, sig_nets_i);
   if (nets == NULL) {
      char *buf = xasprintf("%s_nets",
                            package_signal_path_name(tree_ident(decl)));

      if ((nets = LLVMGetNamedGlobal(module, buf)) == NULL) {
         type_t type = tree_type(decl);
         const int nnets = type_is_array(type)
            ? cgen_const_array_len(type, -1) : 1;

         LLVMTypeRef map_type = LLVMArrayType(cgen_net_id_type(), nnets);
         nets = LLVMAddGlobal(module, map_type, buf);
         LLVMSetLinkage(nets, LLVMExternalLinkage);
      }

      free(buf);
   }

   if (tree_kind(decl) == T_SIGNAL_DECL) {
      // Get a pointer to the first element
      LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
      nets = LLVMBuildGEP(builder, nets, indexes, ARRAY_LEN(indexes), "");
   }

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

static void cgen_call_args(tree_t t, LLVMValueRef *args, unsigned *nargs,
                           type_t *arg_types, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   ident_t builtin = tree_attr_str(decl, ident_new("builtin"));

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);

   for (int i = 0; i < nparams; i++) {
      tree_t value = tree_value(tree_param(t, i));
      type_t type = tree_type(value);

      tree_t port = ((builtin == NULL) || (i < nports))
         ? tree_port(decl, i) : NULL;
      class_t class = (port != NULL) ? tree_class(port) : C_DEFAULT;
      port_mode_t mode = (port != NULL) ? tree_subkind(port) : PORT_IN;

      arg_types[i] = type;

      if ((builtin == NULL) && (class == C_SIGNAL)) {
         // Pass a pointer to the array of nets
         args[i] = cgen_signal_lvalue(value, ctx);

         if (type_is_scalar(type)) {
            cgen_check_scalar_bounds_hint(
               port, value,
               cgen_scalar_vec_load(args[i], type, false, ctx),
               ctx);
         }
      }
      else {
         args[i] = NULL;

         // If this is a scalar out or inout parameter then we need
         // to pass a pointer rather than the value
         if ((builtin == NULL) || (i < nports)) {
            bool need_ptr = (((mode == PORT_OUT)
                              || (mode == PORT_INOUT)
                              || (class == C_FILE))
                             && !type_is_array(type));
            if (need_ptr)
               args[i] = cgen_var_lvalue(value, ctx);
         }

         if (args[i] == NULL) {
            args[i] = cgen_expr(value, ctx);

            if ((builtin == NULL) && type_is_scalar(type))
               cgen_check_scalar_bounds_hint(port, value, args[i], ctx);
         }
      }

      type_t formal_type;
      if ((builtin == NULL) || (i < nports))
         formal_type = tree_type(tree_port(decl, i));
      else
         formal_type = tree_type(value);

      // If we are passing a constrained array argument wrap it in
      // a structure with its metadata. Note we don't need to do
      // this for unconstrained arrays as they are already wrapped.
      const bool need_wrap =
         (type_is_unconstrained(formal_type))
         && cgen_const_bounds(type)
         && (builtin == NULL);

      if (need_wrap) {
         LLVMValueRef data;
         if (class == C_SIGNAL) {
            LLVMValueRef indexes[] = { llvm_int32(0) };
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

   *nargs = nparams;

   if (tree_attr_int(decl, nest_level_i, 0) > 0) {
      // Nested subprograms take extra structure of pointers to
      // parent state

      tree_t parent = (ctx->proc != NULL) ? ctx->proc : ctx->fdecl;

      LLVMValueRef parent_state = LLVMGetUndef(cgen_nest_struct_type(parent));
      unsigned offset = 0;

      const bool chained = (ctx->fdecl != NULL) && (ctx->nest_state != NULL);
      if (chained)
         parent_state = LLVMBuildInsertValue(builder, parent_state,
                                             ctx->nest_state, offset++, "");

      if (ctx->fdecl != NULL) {
         const int nports = tree_ports(ctx->fdecl);
         for (int i = 0; i < nports; i++) {
            tree_t p = tree_port(ctx->fdecl, i);

            LLVMValueRef var = tree_attr_ptr(p, local_var_i);

            type_t type = tree_type(p);
            if (type_is_array(type) && cgen_const_bounds(type)) {
               LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(0) };
               var = LLVMBuildGEP(builder, var,
                                  indexes, ARRAY_LEN(indexes), "");
            }

            parent_state = LLVMBuildInsertValue(
               builder,
               parent_state,
               var,
               offset++,
               istr(tree_ident(p)));
         }
      }

      const int ndecls = tree_decls(parent);
      for (int i = 0; i < ndecls; i++) {
         tree_t d = tree_decl(parent, i);
         switch (tree_kind(d)) {
         case T_CONST_DECL:
         case T_VAR_DECL:
            parent_state = LLVMBuildInsertValue(
               builder,
               parent_state,
               cgen_get_var(d, ctx),
               offset++,
               istr(tree_ident(d)));
            break;
         default:
            break;
         }
      }

      args[(*nargs)++] = parent_state;
   }

   tree_kind_t decl_kind = tree_kind(decl);
   const bool is_proc =
      (decl_kind == T_PROC_DECL) || (decl_kind == T_PROC_BODY);
   if (is_proc) {
      // Final parameter to a procedure is its dynamic context
      args[(*nargs)++] = LLVMConstNull(llvm_void_ptr());
   }
}

static LLVMValueRef cgen_record_eq(LLVMValueRef left, LLVMValueRef right,
                                   type_t type, LLVMIntPredicate pred,
                                   cgen_ctx_t *ctx)
{
   assert(type_is_record(type));

   LLVMValueRef result = llvm_int1(true);

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      LLVMValueRef lfield = LLVMBuildStructGEP(builder, left, i, "lfield");
      LLVMValueRef rfield = LLVMBuildStructGEP(builder, right, i, "rfield");

      LLVMValueRef cmp;
      type_t ftype = tree_type(type_field(type, i));
      if (type_is_array(ftype))
         cmp = cgen_array_rel(lfield, rfield, ftype, ftype, pred, ctx);
      else if (type_is_record(ftype))
         cmp = cgen_record_eq(lfield, rfield, ftype, pred, ctx);
      else {
         LLVMValueRef lload = LLVMBuildLoad(builder, lfield, "");
         LLVMValueRef rload = LLVMBuildLoad(builder, rfield, "");
         cmp = LLVMBuildICmp(builder, pred, lload, rload, "");
      }

      result = LLVMBuildAnd(builder, result, cmp, "");
   }

   return result;
}

static LLVMValueRef cgen_array_rel_inner(LLVMValueRef lhs_data,
                                         LLVMValueRef rhs_data,
                                         LLVMValueRef lhs_array,
                                         LLVMValueRef rhs_array,
                                         type_t left_type, type_t right_type,
                                         LLVMIntPredicate pred, cgen_ctx_t *ctx)
{
   // Behaviour of relational operators on arrays is described in
   // LRM 93 section 7.2.2

   assert((pred == LLVMIntEQ) || (pred == LLVMIntSLT) || (pred == LLVMIntSLE));

   LLVMValueRef left_len  = cgen_array_len(left_type, 0, lhs_array);
   LLVMValueRef right_len = cgen_array_len(right_type, 0, rhs_array);

   LLVMValueRef i = LLVMBuildAlloca(builder, LLVMInt32Type(), "i");
   LLVMBuildStore(builder, llvm_int32(0), i);

   LLVMBasicBlockRef init_bb = LLVMGetInsertBlock(builder);
   LLVMBasicBlockRef test_bb = LLVMAppendBasicBlock(ctx->fn, "rel_test");
   LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(ctx->fn, "rel_body");
   LLVMBasicBlockRef exit_bb = LLVMAppendBasicBlock(ctx->fn, "rel_exit");

   LLVMValueRef len_eq =
      LLVMBuildICmp(builder, LLVMIntEQ, left_len, right_len, "len_eq");

   if (pred == LLVMIntEQ)
      LLVMBuildCondBr(builder, len_eq, test_bb, exit_bb);
   else
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

   type_t elem_type = type_elem(left_type);
   LLVMValueRef cmp, eq;
   if (type_is_array(elem_type)) {
      LLVMValueRef l_indexes[] = { i_loaded, llvm_int32(0) };
      LLVMValueRef l_ptr = LLVMBuildGEP(builder, lhs_data, l_indexes,
                                        ARRAY_LEN(l_indexes), "l_ptr");
      LLVMValueRef r_indexes[] = { i_loaded, llvm_int32(0) };
      LLVMValueRef r_ptr = LLVMBuildGEP(builder, rhs_data, r_indexes,
                                        ARRAY_LEN(r_indexes), "r_ptr");

      cmp = eq = cgen_array_rel_inner(l_ptr, r_ptr, NULL, NULL,
                                      type_elem(left_type),
                                      type_elem(right_type), pred, ctx);
      body_bb = LLVMGetInsertBlock(builder);
   }
   else {
      LLVMValueRef l_ptr = LLVMBuildGEP(builder, lhs_data,
                                        &i_loaded, 1, "l_ptr");
      LLVMValueRef r_ptr = LLVMBuildGEP(builder, rhs_data,
                                        &i_loaded, 1, "r_ptr");

      if (type_is_record(elem_type)) {
         cmp = eq = cgen_record_eq(l_ptr, r_ptr, elem_type, pred, ctx);
         body_bb = LLVMGetInsertBlock(builder);
      }
      else {
         LLVMValueRef l_val = LLVMBuildLoad(builder, l_ptr, "l_val");
         LLVMValueRef r_val = LLVMBuildLoad(builder, r_ptr, "r_val");

         cmp = LLVMBuildICmp(builder, pred, l_val, r_val, "cmp");
         eq  = (pred == LLVMIntEQ) ? cmp
            : LLVMBuildICmp(builder, LLVMIntEQ, l_val, r_val, "eq");
      }
   }

   LLVMValueRef inc =
      LLVMBuildAdd(builder, i_loaded, llvm_int32(1), "inc");
   LLVMBuildStore(builder, inc, i);

   LLVMValueRef i_eq_len =
      LLVMBuildICmp(builder, LLVMIntEQ, inc, left_len, "");
   LLVMValueRef done =
      LLVMBuildOr(builder, LLVMBuildNot(builder, eq, ""),
                  LLVMBuildAnd(builder, len_eq, i_eq_len, ""), "");

   LLVMBuildCondBr(builder, done, exit_bb, test_bb);

   // Epilogue

   LLVMPositionBuilderAtEnd(builder, exit_bb);

   LLVMValueRef phi = LLVMBuildPhi(builder, LLVMInt1Type(), "arel");

   LLVMValueRef      values[] = { cmp,     len_ge_l, len_eq  };
   LLVMBasicBlockRef bbs[]    = { body_bb, test_bb,  init_bb };
   LLVMAddIncoming(phi, values, bbs, (pred == LLVMIntEQ) ? 3 : 2);

   return phi;
}

static LLVMValueRef cgen_array_rel(LLVMValueRef lhs, LLVMValueRef rhs,
                                   type_t left_type, type_t right_type,
                                   LLVMIntPredicate pred, cgen_ctx_t *ctx)
{
   LLVMValueRef left_base  = cgen_array_data_ptr(left_type, lhs);
   LLVMValueRef right_base = cgen_array_data_ptr(right_type, rhs);

   return cgen_array_rel_inner(left_base, right_base, lhs, rhs,
                               left_type, right_type, pred, ctx);
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
   ident_t instance = tree_attr_str(decl, ident_new("INSTANCE_NAME"));
   if (instance == NULL) {
      // Assume this is a package not an elaborated design
      i = ident_new(package_signal_path_name(tree_ident(decl)));
   }
   else {
      switch (which) {
      case PATH_NAME:
         i = tree_ident(decl);
         break;
      case INSTANCE_NAME:
         i = instance;
         break;
      }
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

   unsigned max_width = 0;
   for (int i = 0; i < nargs; i++) {
      const unsigned width = LLVMGetIntTypeWidth(LLVMTypeOf(args[i]));
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
                                   type_t type, LLVMValueRef shift,
                                   cgen_ctx_t *ctx)
{
   ctx->tmp_stack_used = true;

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

static LLVMValueRef cgen_bit_vec_op(bit_vec_op_kind_t kind, type_t left_type,
                                    LLVMValueRef left, type_t right_type,
                                    LLVMValueRef right, cgen_ctx_t *ctx)
{
   ctx->tmp_stack_used = true;

   LLVMValueRef result = LLVMBuildAlloca(builder,
                                         llvm_uarray_type(LLVMInt1Type(), 1),
                                         "bit_vec_op");

   LLVMValueRef right_data = (right == NULL)
      ? LLVMConstNull(LLVMPointerType(LLVMInt1Type(), 0))
      : cgen_array_data_ptr(right_type, right);

   LLVMValueRef right_len = (right == NULL)
      ? llvm_int32(0)
      : cgen_array_len(right_type, 0, right);

   LLVMValueRef right_dir = (right == NULL)
      ? llvm_int8(0)
      : cgen_array_dir(right_type, 0, right);

   LLVMValueRef args[] = {
      llvm_int32(kind),
      cgen_array_data_ptr(left_type, left),
      cgen_array_len(left_type, 0, left),
      cgen_array_dir(left_type, 0, left),
      right_data,
      right_len,
      right_dir,
      result
   };
   LLVMBuildCall(builder, llvm_fn("_bit_vec_op"), args, ARRAY_LEN(args), "");

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

static LLVMValueRef cgen_logical(tree_t t, LLVMValueRef result)
{
   if (tree_attr_int(t, sub_cond_i, 0) > 0) {
      // This is a sub-condition of a Boolean expression being annotated
      // for coverage
      cgen_cond_coverage(t, result);
   }

   return result;
}

static LLVMValueRef cgen_attr_val(tree_t t, LLVMValueRef arg, cgen_ctx_t *ctx)
{
   cgen_check_scalar_bounds(t, arg, ctx);
   return LLVMBuildIntCast(builder, arg, llvm_type(tree_type(t)), "val");
}

static LLVMValueRef cgen_attr_value(tree_t t, LLVMValueRef arg, type_t arg_type,
                                    cgen_ctx_t *ctx)
{
   LLVMValueRef args[] = {
      cgen_array_data_ptr(arg_type, arg),
      cgen_array_len(arg_type, 0, arg),
      llvm_int32(tree_index(t)),
      LLVMBuildPointerCast(builder, mod_name,
                           LLVMPointerType(LLVMInt8Type(), 0), "")
   };
   LLVMValueRef value = LLVMBuildCall(builder, llvm_fn("_value_attr"),
                                      args, ARRAY_LEN(args), "value");

   value = cgen_narrow(tree_type(t), value);

   cgen_check_scalar_bounds(t, value, ctx);

   return value;
}

static LLVMValueRef cgen_fcall(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   assert(tree_kind(decl) == T_FUNC_DECL
          || tree_kind(decl) == T_FUNC_BODY);

   ident_t builtin = tree_attr_str(decl, ident_new("builtin"));

   // Special attributes where the arguments are not always evaluated
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
      else if (icmp(builtin, "left")) {
         tree_t p1 = tree_value(tree_param(t, 1));
         type_t type = tree_type(p1);
         int64_t dim = assume_int(p0);
         if (cgen_const_bounds(type))
            return cgen_expr(type_dim(type, dim).left, ctx);
         else
            return cgen_uarray_field(p1, 0, ctx);
      }
      else if (icmp(builtin, "right")) {
         tree_t p1 = tree_value(tree_param(t, 1));
         type_t type = tree_type(p1);
         int64_t dim = assume_int(p0);
         if (cgen_const_bounds(type))
            return cgen_expr(type_dim(type, dim).right, ctx);
         else
            return cgen_uarray_field(p1, 1, ctx);
      }
      else if (icmp(builtin, "low")) {
         tree_t p1 = tree_value(tree_param(t, 1));
         type_t type = tree_type(p1);
         int64_t dim = assume_int(p0);
         if (cgen_const_bounds(type)) {
            range_t r = type_dim(type, dim);
            return cgen_expr((r.kind == RANGE_TO) ? r.left : r.right, ctx);
         }
         else
            return LLVMBuildSelect(
               builder, cgen_uarray_asc(p1, ctx),
               cgen_uarray_field(p1, 0, ctx),
               cgen_uarray_field(p1, 1, ctx),
               "low");
      }
      else if (icmp(builtin, "high")) {
         tree_t p1 = tree_value(tree_param(t, 1));
         type_t type = tree_type(p1);
         int64_t dim = assume_int(p0);
         if (cgen_const_bounds(type)) {
            range_t r = type_dim(type, dim);
            return cgen_expr((r.kind == RANGE_TO) ? r.right : r.left, ctx);
         }
         else
            return LLVMBuildSelect(
               builder, cgen_uarray_asc(p1, ctx),
               cgen_uarray_field(p1, 1, ctx),
               cgen_uarray_field(p1, 0, ctx),
               "high");
      }
   }

   unsigned nargs;
   const int nparams = tree_params(t);
   LLVMValueRef args[nparams + 1];
   type_t arg_types[nparams + 1];
   cgen_call_args(t, args, &nargs, arg_types, ctx);

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
      else if (icmp(builtin, "mulrp")) {
         LLVMValueRef p =
            LLVMBuildSIToFP(builder, args[1], LLVMDoubleType(), "");
         LLVMValueRef r = LLVMBuildFMul(builder, args[0], p, "");
         return LLVMBuildFPToSI(builder, r, llvm_type(rtype), "");
      }
      else if (icmp(builtin, "mulpr")) {
         LLVMValueRef p =
            LLVMBuildSIToFP(builder, args[0], LLVMDoubleType(), "");
         LLVMValueRef r = LLVMBuildFMul(builder, args[1], p, "");
         return LLVMBuildFPToSI(builder, r, llvm_type(rtype), "");
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
      else if (icmp(builtin, "divpr")) {
         LLVMValueRef p =
            LLVMBuildSIToFP(builder, args[0], LLVMDoubleType(), "");
         LLVMValueRef r = LLVMBuildFDiv(builder, p, args[1], "");
         return LLVMBuildFPToSI(builder, r, llvm_type(rtype), "");
      }
      else if (icmp(builtin, "eq")) {
         if (!real)
            cgen_widen(rtype, args, nparams);
         LLVMValueRef r = real
            ? LLVMBuildFCmp(builder, LLVMRealUEQ, args[0], args[1], "")
            : LLVMBuildICmp(builder, LLVMIntEQ, args[0], args[1], "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "neq")) {
         if (!real)
            cgen_widen(rtype, args, nparams);
         LLVMValueRef r = real
            ? LLVMBuildFCmp(builder, LLVMRealUNE, args[0], args[1], "")
            : LLVMBuildICmp(builder, LLVMIntNE, args[0], args[1], "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "lt")) {
         if (!real)
            cgen_widen(rtype, args, nparams);
         LLVMValueRef r = real
            ? LLVMBuildFCmp(builder, LLVMRealULT, args[0], args[1], "")
            : LLVMBuildICmp(builder, LLVMIntSLT, args[0], args[1], "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "gt")) {
         if (!real)
            cgen_widen(rtype, args, nparams);
         LLVMValueRef r = real
            ? LLVMBuildFCmp(builder, LLVMRealUGT, args[0], args[1], "")
            : LLVMBuildICmp(builder, LLVMIntSGT, args[0], args[1], "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "leq")) {
         if (!real)
            cgen_widen(rtype, args, nparams);
         LLVMValueRef r = real
            ? LLVMBuildFCmp(builder, LLVMRealULE, args[0], args[1], "")
            : LLVMBuildICmp(builder, LLVMIntSLE, args[0], args[1], "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "geq")) {
         if (!real)
            cgen_widen(rtype, args, nparams);
         LLVMValueRef r = real
            ? LLVMBuildFCmp(builder, LLVMRealUGE, args[0], args[1], "")
            : LLVMBuildICmp(builder, LLVMIntSGE, args[0], args[1], "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "neg")) {
         if (real)
            return LLVMBuildFNeg(builder, args[0], "neg");
         else
            return LLVMBuildNeg(builder, args[0], "neg");
      }
      else if (icmp(builtin, "not"))
         return cgen_logical(t, LLVMBuildNot(builder, args[0], ""));
      else if (icmp(builtin, "and"))
         return cgen_logical(t, LLVMBuildAnd(builder, args[0], args[1], ""));
      else if (icmp(builtin, "or"))
         return cgen_logical(t, LLVMBuildOr(builder, args[0], args[1], ""));
      else if (icmp(builtin, "xor"))
         return cgen_logical(t, LLVMBuildXor(builder, args[0], args[1], ""));
      else if (icmp(builtin, "xnor")) {
         LLVMValueRef r =
            LLVMBuildNot(builder,
                         LLVMBuildXor(builder, args[0], args[1], ""), "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "nand")) {
         LLVMValueRef r =
            LLVMBuildNot(builder,
                         LLVMBuildAnd(builder, args[0], args[1], ""), "");
         return cgen_logical(t, r);
      }
      else if (icmp(builtin, "nor")) {
         LLVMValueRef r =
            LLVMBuildNot(builder,
                         LLVMBuildOr(builder, args[0], args[1], ""), "");
         return cgen_logical(t, r);
      }
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
      else if (icmp(builtin, "aneq")) {
         LLVMValueRef eq =
            cgen_array_rel(args[0], args[1], arg_types[0], arg_types[1],
                           LLVMIntEQ, ctx);
         return LLVMBuildNot(builder, eq, "");
      }
      else if (icmp(builtin, "req"))
         return cgen_record_eq(args[0], args[1], arg_types[0], LLVMIntEQ, ctx);
      else if (icmp(builtin, "rneq"))
         return cgen_record_eq(args[0], args[1], arg_types[0], LLVMIntNE, ctx);
      else if (icmp(builtin, "v_not"))
         return cgen_bit_vec_op(BIT_VEC_NOT, arg_types[0],
                                args[0], NULL, NULL, ctx);
      else if (icmp(builtin, "v_and"))
         return cgen_bit_vec_op(BIT_VEC_AND, arg_types[0],
                                args[0], arg_types[1], args[1], ctx);
      else if (icmp(builtin, "v_or"))
         return cgen_bit_vec_op(BIT_VEC_OR, arg_types[0],
                                args[0], arg_types[1], args[1], ctx);
      else if (icmp(builtin, "v_xor"))
         return cgen_bit_vec_op(BIT_VEC_XOR, arg_types[0],
                                args[0], arg_types[1], args[1], ctx);
      else if (icmp(builtin, "v_xnor"))
         return cgen_bit_vec_op(BIT_VEC_XNOR, arg_types[0],
                                args[0], arg_types[1], args[1], ctx);
      else if (icmp(builtin, "v_nand"))
         return cgen_bit_vec_op(BIT_VEC_NAND, arg_types[0],
                                args[0], arg_types[1], args[1], ctx);
      else if (icmp(builtin, "v_nor"))
         return cgen_bit_vec_op(BIT_VEC_NOR, arg_types[0],
                                args[0], arg_types[1], args[1], ctx);
      else if (icmp(builtin, "image")) {
         ctx->tmp_stack_used = true;
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
      else if (icmp(builtin, "agt")) {
         LLVMValueRef leq =
            cgen_array_rel(args[0], args[1], arg_types[0],
                           tree_type(tree_value(tree_param(t, 1))),
                           LLVMIntSLE, ctx);
         return LLVMBuildNot(builder, leq, "");
      }
      else if (icmp(builtin, "aleq"))
         return cgen_array_rel(args[0], args[1], arg_types[0],
                               tree_type(tree_value(tree_param(t, 1))),
                               LLVMIntSLE, ctx);
      else if (icmp(builtin, "ageq")) {
         LLVMValueRef lt =
            cgen_array_rel(args[0], args[1], arg_types[0],
                           tree_type(tree_value(tree_param(t, 1))),
                           LLVMIntSLT, ctx);
         return LLVMBuildNot(builder, lt, "");
      }
      else if (icmp(builtin, "endfile"))
         return LLVMBuildCall(builder, llvm_fn("_endfile"), args, 1, "");
      else if (icmp(builtin, "sll"))
         return cgen_bit_shift(BIT_SHIFT_SLL,
                               args[0], arg_types[0], args[1], ctx);
      else if (icmp(builtin, "srl"))
         return cgen_bit_shift(BIT_SHIFT_SRL,
                               args[0], arg_types[0], args[1], ctx);
      else if (icmp(builtin, "sla"))
         return cgen_bit_shift(BIT_SHIFT_SLA,
                               args[0], arg_types[0], args[1], ctx);
      else if (icmp(builtin, "sra"))
         return cgen_bit_shift(BIT_SHIFT_SRA,
                               args[0], arg_types[0], args[1], ctx);
      else if (icmp(builtin, "rol"))
         return cgen_bit_shift(BIT_SHIFT_ROL,
                               args[0], arg_types[0], args[1], ctx);
      else if (icmp(builtin, "ror"))
         return cgen_bit_shift(BIT_SHIFT_ROR,
                               args[0], arg_types[0], args[1], ctx);
      else if (icmp(builtin, "pos"))
         return LLVMBuildZExt(builder, args[0], llvm_type(rtype), "pos");
      else if (icmp(builtin, "val"))
         return cgen_attr_val(t, args[0], ctx);
      else if (icmp(builtin, "value"))
         return cgen_attr_value(t, args[0], arg_types[0], ctx);
      else if (icmp(builtin, "identity"))
         return args[0];
      else
         fatal("cannot generate code for builtin %s", istr(builtin));
   }
   else {
      ctx->tmp_stack_used = true;
      return LLVMBuildCall(builder, cgen_fdecl(decl, NULL), args, nargs, "");
   }
}

static LLVMValueRef cgen_alias(tree_t alias, cgen_ctx_t *ctx)
{
   assert(tree_kind(alias) == T_ALIAS);

   tree_t value = tree_value(alias);
   LLVMValueRef aliased = cgen_expr(value, ctx);

   // The aliased object may have non-constant bounds whereas the
   // alias itself has known bounds

   type_t alias_type = tree_type(alias);
   type_t value_type = tree_type(value);

   if (!type_is_array(alias_type))
      return aliased;

   const bool alias_const = cgen_const_bounds(alias_type);
   const bool value_const = cgen_const_bounds(value_type);

   if (alias_const && !value_const)
      return cgen_array_data_ptr(value_type, aliased);
   else if (!alias_const) {
      assert(type_dims(alias_type) == 1);  // TODO
      range_t r = type_dim(alias_type, 0);
      LLVMValueRef left  = cgen_expr(r.left, ctx);
      LLVMValueRef right = cgen_expr(r.right, ctx);
      LLVMValueRef kind  = cgen_array_dir(alias_type, 0, NULL);
      LLVMValueRef data  = cgen_array_data_ptr(value_type, aliased);
      return cgen_array_meta_1(alias_type, left, right, kind, data);
   }
   else
      return aliased;
}

static LLVMValueRef cgen_ref(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = tree_ref(t);
   type_t type = tree_type(decl);
   bool needs_load = false;

   tree_kind_t kind = tree_kind(decl);
   switch (kind) {
   case T_ENUM_LIT:
      return LLVMConstInt(llvm_type(type), tree_pos(decl), false);

   case T_CONST_DECL:
   case T_UNIT_DECL:
   case T_VAR_DECL:
   case T_FILE_DECL:
      {
         LLVMValueRef ptr = cgen_get_var(decl, ctx);
         if (type_is_array(type) || type_is_record(type))
            return ptr;
         else
            return LLVMBuildLoad(builder, ptr, "");
      }

   case T_PORT_DECL:
      needs_load = (((tree_subkind(decl) == PORT_INOUT)
                     || (tree_class(decl) == C_FILE))
                    && !type_is_array(type)
                    && !type_is_record(type));
      // Fall-through

   case T_SIGNAL_DECL:
      if (cgen_get_class(decl) == C_SIGNAL) {
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

   case T_ALIAS:
      return cgen_alias(decl, ctx);

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

static LLVMValueRef cgen_array_ref_offset(tree_t t, LLVMValueRef meta,
                                          cgen_ctx_t *ctx)
{
   tree_t value = tree_value(t);
   type_t type = tree_type(value);

   tree_t alias = NULL;
   if (tree_kind(value) == T_REF) {
      tree_t decl = tree_ref(value);
      if (tree_kind(decl) == T_ALIAS)
         alias = decl;
   }

   const bool elide_bounds = tree_attr_int(t, elide_bounds_i, 0);

   LLVMValueRef idx = llvm_int32(0);
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      assert(tree_subkind(p) == P_POS);

      LLVMValueRef offset = cgen_expr(tree_value(p), ctx);

      if (!elide_bounds)
         cgen_check_array_bounds(tree_value(p), type, i, (alias ? NULL : meta),
                                 offset, ctx);

      if (alias != NULL) {
         offset = cgen_unalias_index(alias, offset, meta, ctx);
         type = tree_type(tree_value(alias));
      }

      if (i > 0) {
         LLVMValueRef stride = cgen_array_len(type, i, meta);
         idx = LLVMBuildMul(builder, idx, stride, "stride");
      }

      idx = LLVMBuildAdd(builder, idx,
                         cgen_array_off(offset, meta, type, ctx, i), "idx");
   }

   return idx;
}

static LLVMValueRef cgen_array_ref(tree_t t, cgen_ctx_t *ctx)
{
   tree_t decl = NULL;
   class_t class = C_VARIABLE;
   LLVMValueRef array = NULL;

   tree_t alias = NULL;
   tree_t value = tree_value(t);
   if (tree_kind(value) == T_REF) {
      decl = tree_ref(value);

      if (tree_kind(decl) == T_ALIAS) {
         alias = decl;
         array = cgen_expr(tree_value(decl), ctx);
      }
      else {
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
   }
   else
      array = cgen_expr(value, ctx);

   type_t type = tree_type(value);

   LLVMValueRef idx = cgen_array_ref_offset(t, array, ctx);

   if (alias != NULL)
      array = cgen_alias(alias, ctx);

   switch (class) {
   case C_VARIABLE:
   case C_CONSTANT:
   case C_DEFAULT:
      {
         LLVMValueRef data = cgen_array_data_ptr(type, array);
         LLVMValueRef ptr = LLVMBuildGEP(builder, data, &idx, 1, "");
         type_t rtype = tree_type(t);
         if (type_is_array(rtype) || type_is_record(rtype))
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
         if (type_is_unconstrained(type)) {
            // Unwrap array to nets array
            array = LLVMBuildExtractValue(builder, array, 0, "aptr");
         }

         LLVMValueRef indexes[] = { idx };
         nets = LLVMBuildGEP(builder, array,
                             indexes, ARRAY_LEN(indexes), "");

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

      if (tree_kind(decl) == T_ALIAS) {
         LLVMValueRef array = cgen_alias(decl, ctx);
         return cgen_get_slice(array, type, decl, tree_range(t), ctx);

      }
      else {
         switch (cgen_get_class(decl)) {
         case C_VARIABLE:
         case C_DEFAULT:
         case C_CONSTANT:
            {
               LLVMValueRef array = cgen_get_var(decl, ctx);
               return cgen_get_slice(array, type, NULL, tree_range(t), ctx);
            }

         case C_SIGNAL:
            return cgen_vec_load(cgen_signal_nets(decl), type, tree_type(t),
                                 false, ctx);

         default:
            assert(false);
         }
      }
   }
   else {
      LLVMValueRef src = cgen_expr(value, ctx);
      return cgen_get_slice(src, tree_type(value), NULL, tree_range(t), ctx);
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

   range_t r = type_dim(type, dim);
   const int64_t left = assume_int(r.left);
   const bool is_downto = (r.kind == RANGE_DOWNTO);

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      tree_t value = tree_value(a);

      LLVMValueRef *sub;
      int nsub;
      if (dim < ndims - 1)
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
            free(v);
         }
         else if (type_is_record(sub_type)) {
            *sub = cgen_const_record(value, true, ctx);
         }
         else
            assert(false);
      }
      else {
         sub  = xmalloc(sizeof(LLVMValueRef));
         *sub = cgen_expr(value, ctx);
         nsub = 1;
      }

      switch (tree_subkind(a)) {
      case A_POS:
         cgen_copy_vals(vals + (i * nsub), sub, nsub, false);
         break;

      case A_NAMED:
         {
            const int64_t name = assume_int(tree_name(a));
            const int64_t off  = is_downto ? left - name : name - left;
            cgen_copy_vals(vals + (off * nsub), sub, nsub, false);
         }
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

            for (int j = r_low; j <= r_high; j++) {
               const int64_t off = is_downto ? left - j : j - left;
               cgen_copy_vals(vals + (off * nsub), sub, nsub, false);
            }
         }
         break;
      }

      free(sub);
   }

   for (int i = 0; i < *n_elems; i++)
      assert(vals[i] != NULL);

   return vals;
}

static LLVMValueRef cgen_dyn_aggregate(tree_t t, bool use_tmp, cgen_ctx_t *ctx)
{
   // Generate code to fill in the aggregate at run time

   type_t type = tree_type(t);

   LLVMBasicBlockRef test_bb  = LLVMAppendBasicBlock(ctx->fn, "da_test");
   LLVMBasicBlockRef body_bb  = LLVMAppendBasicBlock(ctx->fn, "da_body");
   LLVMBasicBlockRef exit_bb  = LLVMAppendBasicBlock(ctx->fn, "da_exit");

   // Prelude
   LLVMValueRef a = cgen_tmp_var(type, "dyn_tmp", use_tmp, ctx);
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

   LLVMValueRef dir = cgen_array_dir(type, 0, a);
   LLVMValueRef is_downto = LLVMBuildICmp(builder, LLVMIntEQ, dir,
                                          llvm_int8(RANGE_DOWNTO), "is_downto");

   LLVMValueRef left = cgen_array_left(type, 0, a);

   LLVMBuildBr(builder, test_bb);

   // Loop test
   LLVMPositionBuilderAtEnd(builder, test_bb);
   LLVMValueRef i_loaded = LLVMBuildLoad(builder, i, "i");
   LLVMValueRef ge = LLVMBuildICmp(builder, LLVMIntUGE, i_loaded, len, "ge");
   LLVMBuildCondBr(builder, ge, exit_bb, body_bb);

   // Loop body
   LLVMPositionBuilderAtEnd(builder, body_bb);

   LLVMValueRef what = def;
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      assoc_kind_t kind = tree_subkind(a);
      LLVMValueRef val = NULL;
      if (kind != A_OTHERS) {
         val = cgen_expr(tree_value(a), ctx);
         if (def == NULL)
            what = def = LLVMGetUndef(LLVMTypeOf(val));
      }

      switch (kind) {
      case A_POS:
         {
            LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ, i_loaded,
                                            llvm_int32(tree_pos(a)), "");
            what = LLVMBuildSelect(builder, eq, val, what, "");
         }
         break;

      case A_NAMED:
         {
            LLVMValueRef name = cgen_expr(tree_name(a), ctx);
            LLVMValueRef off  =
               LLVMBuildSelect(builder, is_downto,
                               LLVMBuildSub(builder, left, name, ""),
                               LLVMBuildSub(builder, name, left, ""), "off");

            LLVMValueRef eq = LLVMBuildICmp(builder, LLVMIntEQ,
                                            i_loaded, off, "");
            what = LLVMBuildSelect(builder, eq, val, what, "");
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

            what = LLVMBuildSelect(builder, in, val, what, "");
         }
         break;

      case A_OTHERS:
         break;
      }
   }

   if (type_is_array(assoc_type)) {
      type_t dst_type = (type_dims(type) == 1) ? type_elem(type) : type;
      cgen_array_copy(assoc_type, dst_type, what, a, i_loaded, ctx);
   }
   else {
      LLVMValueRef indexes[] = { i_loaded };
      LLVMValueRef ptr = LLVMBuildGEP(builder, data, indexes,
                                      ARRAY_LEN(indexes), "ptr");
      if (type_is_record(assoc_type))
         cgen_record_copy(assoc_type, what, ptr);
      else
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

static LLVMValueRef cgen_const_record(tree_t t, bool nest, cgen_ctx_t *ctx)
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

   LLVMTypeRef lltype = llvm_type(type);
   LLVMValueRef init = LLVMConstNamedStruct(lltype, vals, nfields);

   if (nest || (ctx == NULL))
      return init;
   else {
      LLVMValueRef mem = LLVMBuildAlloca(builder, lltype, "const_rec");

      LLVMBuildStore(builder, init, mem);

      return mem;
   }
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
         return cgen_dyn_aggregate(t, false, ctx);
   }
   else if (type_is_record(type)) {
      if (cgen_is_const(t))
         return cgen_const_record(t, false, ctx);
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
   if (type_is_unconstrained(type)) {
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
      var = cgen_tmp_var(tree_type(t), "concat", false, ctx);

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

   type_t type = tree_type(t);

   LLVMValueRef rec = cgen_expr(value, ctx);
   LLVMValueRef field = LLVMBuildStructGEP(builder, rec, index, "field");
   if (type_is_array(type) || type_is_record(type))
      return field;
   else
      return LLVMBuildLoad(builder, field, "");
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

      LLVMValueRef left, right, dir;
      if (type_is_unconstrained(value_type)) {
         left  = cgen_array_left(value_type, 0, init);
         right = cgen_array_right(value_type, 0, init);
         dir   = cgen_array_dir(value_type, 0, init);
      }
      else {
         range_t r = type_dim(value_type, 0);
         left  = cgen_expr(r.left, ctx);
         right = cgen_expr(r.right, ctx);
         dir   = llvm_int8(r.kind);
      }

      LLVMTypeRef elem = llvm_type(type_elem(type));

      LLVMValueRef meta =
         cgen_array_meta_1(type, left, right, dir,
                           LLVMConstNull(LLVMPointerType(elem, 0)));

      LLVMValueRef len  = cgen_array_len(type, -1, meta);
      LLVMValueRef data = LLVMBuildArrayMalloc(builder, elem, len, "data");

      meta = LLVMBuildInsertValue(builder, meta, data, 0, "");

      LLVMBuildStore(builder, meta, ptr);

      cgen_array_copy(value_type, type, init, meta, NULL, ctx);
   }
   else if (type_is_array(type))
      cgen_array_copy(value_type, value_type, init, ptr, NULL, ctx);
   else if (type_is_record(type))
      cgen_record_copy(value_type, init, ptr);
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
   if ((type_is_array(type) && cgen_const_bounds(type))
       || type_is_record(type))
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

static void cgen_sched_event(tree_t on, bool is_static, cgen_ctx_t *ctx)
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
   if (kind == T_ALIAS) {
      cgen_sched_event(tree_value(decl), is_static, ctx);
      return;
   }
   else if ((kind != T_SIGNAL_DECL) && (kind != T_PORT_DECL)) {
      // As above, a port could have been rewritten to reference a
      // constant declaration or enumeration literal, in which case
      // just ignore it too
      return;
   }

   type_t type = tree_type(decl);
   type_t expr_type = tree_type(on);

   const bool array = type_is_array(type);

   LLVMValueRef n_elems, nets;
   bool sequential = false;
   if (expr_kind == T_REF) {
      nets = cgen_signal_nets(decl);

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
         netid_t last = NETID_INVALID;
         for (i = 0; i < nnets; i++) {
            const netid_t nid = tree_net(decl, i);
            if ((last == NETID_INVALID) || (nid == last + 1))
               last = nid;
            else
               break;
         }

         sequential = (i == nnets);
      }
   }
   else {
      assert(array);
      nets = cgen_signal_lvalue(on, ctx);
      assert(nets != NULL);

      if (type_is_array(expr_type))
         n_elems = cgen_array_len_recur(expr_type, NULL);
      else
         n_elems = llvm_int32(1);
   }

   const int flags =
      (sequential ? SCHED_SEQUENTIAL : 0)
      | (is_static ? SCHED_STATIC : 0);

   LLVMValueRef args[] = {
      llvm_void_cast(nets),
      n_elems,
      llvm_int32(flags),
   };
   LLVMBuildCall(builder, llvm_fn("_sched_event"),
                 args, ARRAY_LEN(args), "");
}

static LLVMValueRef cgen_now(void)
{
   return LLVMBuildCall(builder, llvm_fn("_std_standard_now"), NULL, 0, "");
}

static void cgen_wait(tree_t t, cgen_ctx_t *ctx)
{
   const bool is_static = tree_attr_int(t, static_i, 0);

   assert(!is_static || (!tree_has_delay(t) && !tree_has_value(t)));

   if (tree_has_delay(t)) {
      LLVMValueRef after = cgen_expr(tree_delay(t), ctx);
      cgen_sched_process(after);

      LLVMValueRef ptr = LLVMBuildStructGEP(builder, ctx->state, 2, "");
      LLVMValueRef timeout =
         LLVMBuildAdd(builder, cgen_now(), after, "timeout");
      LLVMBuildStore(builder, timeout, ptr);
   }

   LLVMBasicBlockRef after_bb = NULL;
   if (is_static) {
      // This process is always sensitive to the same set of signals so
      // only call _sched_event once at startup
      // We reuse the 64-bit wait until timeout field as a flag bit

      after_bb = LLVMAppendBasicBlock(ctx->fn, "static_skip");

      LLVMValueRef ptr = LLVMBuildStructGEP(builder, ctx->state, 2, "");
      LLVMValueRef done =
         LLVMBuildICmp(builder,
                       LLVMIntEQ,
                       LLVMBuildLoad(builder, ptr, ""),
                       llvm_int64(1),
                       "static_done");

      LLVMBasicBlockRef static_bb =
         LLVMAppendBasicBlock(ctx->fn, "static_sched");
      LLVMBuildCondBr(builder, done, after_bb, static_bb);

      LLVMPositionBuilderAtEnd(builder, static_bb);
      LLVMBuildStore(builder, llvm_int64(1), ptr);
   }

   const int ntriggers = tree_triggers(t);
   for (int i = 0; i < ntriggers; i++)
      cgen_sched_event(tree_trigger(t, i), is_static, ctx);

   if (after_bb != NULL) {
      LLVMBuildBr(builder, after_bb);
      LLVMPositionBuilderAtEnd(builder, after_bb);
   }

   // Find the basic block to jump to when the process is next scheduled
   proc_entry_t *it;
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

   if (tree_has_value(t)) {
      // Generate code to loop until condition is met

      LLVMValueRef until = cgen_expr(tree_value(t), ctx);

      const bool has_delay = tree_has_delay(t);

      LLVMValueRef done, remain = NULL;
      if (has_delay) {
         LLVMValueRef ptr = LLVMBuildStructGEP(builder, ctx->state, 2, "");
         LLVMValueRef timeout = LLVMBuildLoad(builder, ptr, "");

         remain = LLVMBuildSub(builder, timeout, cgen_now(), "");

         LLVMValueRef expired =
            LLVMBuildICmp(builder, LLVMIntEQ, remain, llvm_int64(0), "");

         done = LLVMBuildOr(builder, until, expired, "");
      }
      else
         done = until;

      LLVMBasicBlockRef again_bb = LLVMAppendBasicBlock(ctx->fn, "again");
      LLVMBasicBlockRef done_bb  = LLVMAppendBasicBlock(ctx->fn, "done");

      LLVMBuildCondBr(builder, done, done_bb, again_bb);

      LLVMPositionBuilderAtEnd(builder, again_bb);

      if (has_delay)
         cgen_sched_process(remain);

      const int ntriggers = tree_triggers(t);
      for (int i = 0; i < ntriggers; i++)
         cgen_sched_event(tree_trigger(t, i), false, ctx);

      if (ctx->proc == NULL)
         LLVMBuildRet(builder, llvm_void_cast(ctx->state));
      else
         LLVMBuildRetVoid(builder);

      LLVMPositionBuilderAtEnd(builder, done_bb);
   }
}

static LLVMValueRef cgen_var_lvalue(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         if (tree_kind(decl) == T_ALIAS)
            return cgen_var_lvalue(tree_value(decl), ctx);
         else
            return cgen_get_var(tree_ref(t), ctx);
      }

   case T_ARRAY_REF:
      {
         type_t type = tree_type(tree_value(t));

         LLVMValueRef var = cgen_var_lvalue(tree_value(t), ctx);

         const bool elide_bounds = tree_attr_int(t, elide_bounds_i, 0);

         LLVMValueRef idx = llvm_int32(0);
         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_param(t, i);
            assert(tree_subkind(p) == P_POS);

            LLVMValueRef off = cgen_expr(tree_value(p), ctx);

            if (!elide_bounds)
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
         return cgen_get_slice(array, ty, NULL, tree_range(t), ctx);
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

   case T_FCALL:
   case T_AGGREGATE:
      return cgen_expr(t, ctx);

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
   if ((base_k == T_INTEGER) && !tree_attr_int(t, elide_bounds_i, 0)) {
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

   if (type_is_array(target_type)) {
      cgen_check_array_sizes(value, target_type, value_type, lhs, rhs, ctx);
      cgen_array_copy(value_type, target_type, rhs, lhs, NULL, ctx);
   }
   else if (type_is_record(target_type))
      cgen_record_copy(target_type, rhs, lhs);
   else
      LLVMBuildStore(builder, rhs, lhs);
}

static LLVMValueRef cgen_signal_lvalue(tree_t t, cgen_ctx_t *ctx)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         if (tree_kind(decl) == T_ALIAS)
            return cgen_signal_lvalue(tree_value(decl), ctx);
         else
            return cgen_array_signal_ptr(decl, llvm_int32(0), ctx);
      }

   case T_ARRAY_REF:
      {
         type_t type = tree_type(tree_value(t));

         if (tree_kind(tree_value(t)) == T_REF) {
            if (type_is_unconstrained(type)) {
               assert(tree_params(t) == 1);

               tree_t decl = tree_ref(tree_value(t));
               assert(type_is_array(tree_type(decl)));

               LLVMValueRef meta = cgen_signal_nets(decl);
               LLVMValueRef off  = cgen_array_ref_offset(t, meta, ctx);

               LLVMValueRef sig_array = cgen_array_data_ptr(type, meta);

               LLVMValueRef indexes[] = { off };
               return LLVMBuildGEP(builder, sig_array,
                                   indexes, ARRAY_LEN(indexes), "");
            }
            else {
               LLVMValueRef off = cgen_array_ref_offset(t, NULL, ctx);

               tree_t decl = tree_ref(tree_value(t));
               assert(type_is_array(tree_type(decl)));

               return cgen_array_signal_ptr(decl, off, ctx);
            }
         }
         else {
            assert(type_kind(type) != T_UARRAY);

            LLVMValueRef off = cgen_array_ref_offset(t, NULL, ctx);

            LLVMValueRef p_base = cgen_signal_lvalue(tree_value(t), ctx);
            if (p_base == NULL)
               return NULL;

            LLVMValueRef indexes[] = { off };
            return LLVMBuildGEP(builder, p_base,
                                indexes, ARRAY_LEN(indexes), "");
         }
      }

   case T_ARRAY_SLICE:
      {
         tree_t value = tree_value(t);

         LLVMValueRef nets = cgen_signal_lvalue(value, ctx);
         if (nets == NULL)
            return NULL;

         range_t r = tree_range(t);

         LLVMValueRef left  = cgen_expr(r.left, ctx);
         LLVMValueRef right = cgen_expr(r.right, ctx);

         type_t val_type = tree_type(tree_value(t));

         cgen_check_array_bounds(r.left, val_type, 0, nets, left, ctx);
         cgen_check_array_bounds(r.right, val_type, 0, nets, right, ctx);

         LLVMValueRef off = cgen_array_off(left, nets, val_type, ctx, 0);

         if (!cgen_const_bounds(val_type))
            nets = LLVMBuildExtractValue(builder, nets, 0, "");

         LLVMValueRef slice = LLVMBuildGEP(builder, nets, &off, 1, "slice");

         type_t type = tree_type(t);
         if (cgen_const_bounds(type))
            return slice;
         else
            return cgen_array_meta_1(NULL, left, right,
                                     llvm_int8(r.kind), slice);
      }

   case T_AGGREGATE:
      {
         const int nassocs = tree_assocs(t);

         LLVMTypeRef nid_type = cgen_net_id_type();
         LLVMValueRef nid_array =
            LLVMBuildArrayAlloca(builder, nid_type, llvm_int32(nassocs),
                                 "aggregate_nid_array");

         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(t, i);
            assert(tree_subkind(a) == A_POS);

            tree_t value = tree_value(a);
            assert(!type_is_array(tree_type(value)));

            LLVMValueRef nid, nid_ptr = cgen_signal_lvalue(value, ctx);
            if (nid_ptr == NULL)
               nid = llvm_int32(NETID_INVALID);
            else
               nid = LLVMBuildLoad(builder, nid_ptr, "nid");

            LLVMValueRef indexes[] = {
               llvm_int32(i)
            };
            LLVMValueRef aptr = LLVMBuildGEP(builder, nid_array,
                                             indexes, ARRAY_LEN(indexes), "");
            LLVMBuildStore(builder, nid, aptr);
         }

         return nid_array;
      }

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
      if (nets == NULL)
         continue;    // Assignment to OPEN port

      LLVMValueRef rhs_data, lhs_data, n_elems, elem_size;

      type_t value_type  = tree_type(value);
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

      if (!type_is_array(value_type)) {
         // Need to pass a pointer to values so allocate this on the stack
         LLVMValueRef tmp = LLVMBuildAlloca(builder, llvm_type(value_type), "");
         LLVMBuildStore(builder, rhs, tmp);

         rhs_data  = tmp;
         lhs_data  = nets;
         n_elems   = llvm_int32(1);
         elem_size = llvm_sizeof(llvm_type(value_type));
      }
      else {
         cgen_check_array_sizes(t, target_type, value_type, nets, rhs, ctx);

         rhs_data = cgen_array_data_ptr(value_type, rhs);
         lhs_data = cgen_array_data_ptr(target_type, nets);

         n_elems   = cgen_array_len_recur(value_type, rhs);
         elem_size = cgen_array_elem_size(value_type);
      }

      LLVMValueRef args[] = {
         llvm_void_cast(lhs_data),
         llvm_void_cast(rhs_data),
         n_elems,
         elem_size,
         after,
         (i == 0) ? reject : llvm_int64(0)
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

static void cgen_cond_coverage(tree_t t, LLVMValueRef value)
{
   const int cover_tag = tree_attr_int(t, cond_tag_i, -1);
   if (cover_tag == -1)
      return;

   const int sub_cond = tree_attr_int(t, sub_cond_i, 0);

   LLVMValueRef cover_conds = LLVMGetNamedGlobal(module, "cover_conds");

   LLVMValueRef indexes[] = { llvm_int32(0), llvm_int32(cover_tag) };
   LLVMValueRef mask_ptr = LLVMBuildGEP(builder, cover_conds,
                                        indexes, ARRAY_LEN(indexes), "");

   LLVMValueRef mask = LLVMBuildLoad(builder, mask_ptr, "cover_conds");

   // Bit zero means evaluated false, bit one means evaluated true
   // Other bits may be used in the future for sub-conditions

   LLVMValueRef or = LLVMBuildSelect(builder, value,
                                     llvm_int32(1 << ((sub_cond * 2) + 1)),
                                     llvm_int32(1 << (sub_cond * 2)),
                                     "cond_mask_or");

   LLVMValueRef mask1 = LLVMBuildOr(builder, mask, or, "");

   LLVMBuildStore(builder, mask1, mask_ptr);
}

static void cgen_if(tree_t t, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef then_bb = LLVMAppendBasicBlock(ctx->fn, "then");
   LLVMBasicBlockRef else_bb = LLVMAppendBasicBlock(ctx->fn, "else");

   LLVMBasicBlockRef end_bb =
      (tree_else_stmts(t) > 0)
      ? LLVMAppendBasicBlock(ctx->fn, "ifend")
      : else_bb;

   tree_t value = tree_value(t);
   LLVMValueRef test = cgen_expr(value, ctx);
   cgen_cond_coverage(value, test);

   LLVMBuildCondBr(builder, test, then_bb, else_bb);

   LLVMPositionBuilderAtEnd(builder, then_bb);

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
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

static LLVMValueRef cgen_tmp_alloc(LLVMValueRef bytes, LLVMTypeRef type)
{
   LLVMValueRef _tmp_stack_ptr = LLVMGetNamedGlobal(module, "_tmp_stack");
   LLVMValueRef _tmp_alloc_ptr = LLVMGetNamedGlobal(module, "_tmp_alloc");

   LLVMValueRef alloc = LLVMBuildLoad(builder, _tmp_alloc_ptr, "alloc");
   LLVMValueRef stack = LLVMBuildLoad(builder, _tmp_stack_ptr, "stack");

   LLVMValueRef indexes[] = { alloc };
   LLVMValueRef buf = LLVMBuildGEP(builder, stack,
                                   indexes, ARRAY_LEN(indexes), "");

   // TODO: align here
   LLVMValueRef alloc_next =
      LLVMBuildAdd(builder, alloc, bytes, "alloc_next");

   LLVMBuildStore(builder, alloc_next, _tmp_alloc_ptr);

   return LLVMBuildPointerCast(builder, buf,
                               LLVMPointerType(type, 0), "tmp_buf");
}

static void cgen_return(tree_t t, cgen_ctx_t *ctx)
{
   if (tree_has_value(t)) {
      LLVMValueRef rval = cgen_expr(tree_value(t), ctx);

      tree_t value = tree_value(t);
      type_t stype = tree_type(value);

      // If we are returning an array then wrap it with metadata
      if (type_is_array(stype)) {
         type_t rtype = type_result(tree_type(ctx->fdecl));

         if ((tree_kind(value) == T_REF) &&
             tree_attr_int(tree_ref(value), returned_i, 0)) {
            // This array was already allocatated in the temporary area

            const bool rconst = cgen_const_bounds(rtype);
            const bool sconst = cgen_const_bounds(stype);

            if (rconst == sconst)
               LLVMBuildRet(builder, rval);
            else if (rconst) {
               LLVMValueRef data = cgen_array_data_ptr(stype, rval);
               LLVMBuildRet(builder, data);
            }
            else if (sconst) {
               LLVMValueRef rarray = cgen_array_meta_1(
                  rtype,
                  cgen_array_left(stype, 0, rval),
                  cgen_array_right(stype, 0, rval),
                  cgen_array_dir(stype, 0, rval),
                  cgen_array_data_ptr(stype, rval));
               LLVMBuildRet(builder, rarray);
            }
            else
               assert(false);
         }
         else {
            // Need to make a copy of this array as it is currently
            // on the stack

            LLVMTypeRef base_type = llvm_type(type_elem(stype));
            LLVMValueRef bytes =
               LLVMBuildMul(builder, cgen_array_len(stype, -1, rval),
                            llvm_sizeof(base_type), "bytes");
            LLVMValueRef buf_ptr = cgen_tmp_alloc(bytes, base_type);

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
      }
      else if (type_is_record(stype)) {
         // Returning a record currently on the stack

         LLVMTypeRef lltype = llvm_type(stype);
         LLVMValueRef copy = cgen_tmp_alloc(llvm_sizeof(lltype), lltype);

         cgen_record_copy(stype, rval, copy);

         LLVMBuildRet(builder, copy);
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
      tree_t value = tree_value(t);
      LLVMValueRef test = cgen_expr(value, ctx);
      cgen_cond_coverage(value, test);

      LLVMBuildCondBr(builder, test, body_bb, exit_bb);
   }
   else
      LLVMBuildBr(builder, body_bb);

   block_list_t *bl = xmalloc(sizeof(block_list_t));
   bl->exit_bb  = exit_bb;
   bl->entry_bb = test_bb;
   bl->name     = tree_ident(t);
   bl->next     = ctx->blocks;

   ctx->blocks = bl;

   LLVMPositionBuilderAtEnd(builder, body_bb);
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      cgen_stmt(tree_stmt(t, i), ctx);
   LLVMBuildBr(builder, test_bb);

   LLVMPositionBuilderAtEnd(builder, exit_bb);

   ctx->blocks = bl->next;
   free(bl);
}

static void cgen_block(tree_t t, cgen_ctx_t *ctx)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      cgen_stmt(tree_stmt(t, i), ctx);
}

static void cgen_loop_control(tree_t t, cgen_ctx_t *ctx)
{
   LLVMBasicBlockRef false_bb = LLVMAppendBasicBlock(ctx->fn, "c_false");

   if (tree_has_value(t)) {
      LLVMBasicBlockRef true_bb = LLVMAppendBasicBlock(ctx->fn, "c_true");

      tree_t value = tree_value(t);
      LLVMValueRef test = cgen_expr(value, ctx);
      cgen_cond_coverage(value, test);

      LLVMBuildCondBr(builder, test, true_bb, false_bb);
      LLVMPositionBuilderAtEnd(builder, true_bb);
   }

   ident_t label = tree_ident2(t);
   block_list_t *bl;
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
      int64_t this = 0;
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
            if (tree_pos(a) == (unsigned)depth) {
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

      unsigned nargs;
      LLVMValueRef args[nparams + 2];
      type_t arg_types[nparams + 2];
      cgen_call_args(t, args, &nargs, arg_types, ctx);

      if (builtin != NULL)
         cgen_builtin_pcall(builtin, args, arg_types);
      else {
         // Regular procedure call
         LLVMBuildCall(builder, cgen_pdecl(decl), args, nargs, "");
      }
   }
   else {
      // Find the basic block to jump to when the procedure resumes
      proc_entry_t *it;
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

      unsigned nargs;
      LLVMValueRef args[nparams + 2];
      type_t arg_types[nparams + 2];
      cgen_call_args(t, args, &nargs, arg_types, ctx);
      args[nargs - 1] = LLVMBuildLoad(builder, context_ptr, "context");

      LLVMValueRef ret =
         LLVMBuildCall(builder, cgen_pdecl(decl), args, nargs, "");

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
      return;
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

   if (ctx->tmp_stack_used && (ctx->fdecl == NULL)) {
      LLVMValueRef _tmp_alloc_ptr =
         LLVMGetNamedGlobal(module, "_tmp_alloc");
      LLVMBuildStore(builder, llvm_int32(0), _tmp_alloc_ptr);

      ctx->tmp_stack_used = false;
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

   proc_entry_t *p = xmalloc(sizeof(proc_entry_t));
   p->next = NULL;
   p->wait = t;
   p->bb   = NULL;

   if (ctx->entry_list == NULL) {
      p->state_num = 1;
      ctx->entry_list = p;
   }
   else {
      proc_entry_t *it;
      for (it = ctx->entry_list; it->next != NULL; it = it->next)
         ;
      p->state_num = it->state_num + 1;
      it->next = p;
   }
}

static LLVMTypeRef *cgen_state_type_fields(tree_t t, unsigned *nfields)
{
   int count = 0;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_CONST_DECL:
      case T_VAR_DECL:
      case T_FILE_DECL:
         count++;
         break;
      default:
         break;
      }
   }

   LLVMTypeRef *fields = xmalloc((count + 3) * sizeof(LLVMTypeRef));
   fields[0] = LLVMInt32Type();   // State
   fields[1] = llvm_void_ptr();   // Procedure dynamic context
   fields[2] = LLVMInt64Type();   // Wait until timeout

   unsigned offset = 3;
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_CONST_DECL:
      case T_VAR_DECL:
      case T_FILE_DECL:
         {
            fields[offset] = llvm_type(tree_type(d));
            tree_add_attr_int(d, var_offset_i, offset);
            offset++;
         }
         break;
      default:
         break;
      }
   }

   *nfields = offset;
   return fields;
}

static LLVMTypeRef cgen_process_state_type(tree_t t)
{
   unsigned nfields;
   LLVMTypeRef *fields = cgen_state_type_fields(t, &nfields);

   char *name = xasprintf("%s__state_s", istr(tree_ident(t)));
   LLVMTypeRef ty = LLVMStructCreateNamed(LLVMGetGlobalContext(), name);
   if (ty == NULL)
      fatal("failed to add type name %s", name);
   LLVMStructSetBody(ty, fields, nfields, false);

   free(name);
   free(fields);
   return ty;
}

static void cgen_jump_table(cgen_ctx_t *ctx, LLVMBasicBlockRef default_bb)
{
   LLVMValueRef state_ptr = LLVMBuildStructGEP(builder, ctx->state, 0, "");
   LLVMValueRef jtarget = LLVMBuildLoad(builder, state_ptr, "");
   LLVMValueRef jswitch = LLVMBuildSwitch(builder, jtarget, default_bb, 10);

   LLVMAddCase(jswitch, llvm_int32(0), default_bb);

   proc_entry_t *it;
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
      tree_kind_t kind = tree_kind(v);
      if ((kind == T_VAR_DECL) || (kind == T_CONST_DECL)
          || (kind == T_FILE_DECL)) {
         if (!tree_has_value(v))
            continue;

         tree_t value = tree_value(v);
         LLVMValueRef val = cgen_expr(value, ctx);

         type_t decl_type  = tree_type(v);
         type_t value_type = tree_type(value);

         if (type_is_array(decl_type)) {
            if (!cgen_const_bounds(decl_type)) {
               // Also store the meta data in the state structure
               // The array data will currently be allocated on the stack so
               // copy into into a malloc-ed area so its value will be preserved
               // when the process or procedure suspends

               LLVMTypeRef elem  = llvm_type(type_elem(decl_type));
               LLVMValueRef len  = cgen_array_len(decl_type, -1, val);
               LLVMValueRef data = LLVMBuildArrayMalloc(builder, elem, len,
                                                        "proc_array");

               LLVMValueRef new_meta =
                  LLVMBuildInsertValue(builder, val, data, 0, "new_meta");

               cgen_array_copy(value_type, decl_type, val, new_meta, NULL, ctx);

               int offset = tree_attr_int(v, var_offset_i, -1);
               assert(offset != -1);

               LLVMValueRef meta_ptr =
                  LLVMBuildStructGEP(builder, ctx->state, offset, "meta_ptr");
               LLVMBuildStore(builder, new_meta, meta_ptr);
            }
            else {
               LLVMValueRef var_ptr = cgen_get_var(v, ctx);
               cgen_check_array_sizes(v, decl_type, value_type,
                                      var_ptr, val, ctx);
               cgen_array_copy(value_type, decl_type, val, var_ptr, NULL, ctx);
            }
         }
         else if (type_is_record(decl_type))
            cgen_record_copy(decl_type, val, cgen_get_var(v, ctx));
         else {
            cgen_check_scalar_bounds(value, val, ctx);
            LLVMBuildStore(builder, val, cgen_get_var(v, ctx));
         }
      }
   }
}

static void cgen_nested_subprograms(tree_t t)
{
   const int level = tree_attr_int(t, nest_level_i, 0);

   int offset = (level == 0) ? 0 : 1;

   if (tree_kind(t) != T_PROCESS) {
      const int nports = tree_ports(t);
      for (int i = 0; i < nports; i++) {
         tree_t p = tree_port(t, i);
         tree_add_attr_int(p, nest_offset_i, offset++);
         tree_add_attr_int(p, nest_level_i, level + 1);
      }
   }

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      tree_kind_t kind = tree_kind(d);

      switch (kind) {
      case T_PROC_BODY:
      case T_FUNC_BODY:
         {
            tree_set_ident(d, ident_prefix(tree_ident(t), tree_ident(d), '.'));

            tree_add_attr_int(d, nest_level_i, level + 1);
            tree_add_attr_tree(d, nest_parent_i, t);

            if (kind == T_PROC_BODY)
               cgen_proc_body(d, t);
            else
               cgen_func_body(d, t);
         }
         break;

      case T_CONST_DECL:
      case T_VAR_DECL:
         tree_add_attr_int(d, nest_offset_i, offset++);
         tree_add_attr_int(d, nest_level_i, level + 1);
         break;

      default:
         break;
      }
   }
}

static void cgen_process(tree_t t)
{
   assert(tree_kind(t) == T_PROCESS);

   cgen_nested_subprograms(t);

   cgen_ctx_t ctx = {
      .entry_list = NULL,
      .proc       = t
   };

   // Create a global structure to hold process state
   char *state_name = xasprintf("%s__state", istr(tree_ident(t)));
   LLVMTypeRef state_ty = cgen_process_state_type(t);
   ctx.state = LLVMAddGlobal(module, state_ty, state_name);
   LLVMSetLinkage(ctx.state, LLVMInternalLinkage);
   free(state_name);

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

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      tree_visit(tree_stmt(t, i), cgen_jump_table_fn, &ctx);

   if (ctx.entry_list == NULL)
      warn_at(tree_loc(t), "no wait statement in process");

   cgen_jump_table(&ctx, start_bb);

   LLVMPositionBuilderAtEnd(builder, init_bb);

   // Variable initialisation

   cgen_proc_var_init(t, &ctx);

   // Return to simulation kernel after initialisation

   LLVMValueRef state_ptr   = LLVMBuildStructGEP(builder, ctx.state, 0, "");
   LLVMValueRef context_ptr = LLVMBuildStructGEP(builder, ctx.state, 1, "");
   LLVMValueRef wait_ptr    = LLVMBuildStructGEP(builder, ctx.state, 2, "");

   cgen_sched_process(llvm_int64(0));
   LLVMBuildStore(builder, llvm_int32(0 /* start */), state_ptr);
   LLVMBuildStore(builder, LLVMConstNull(llvm_void_ptr()), context_ptr);
   LLVMBuildStore(builder, llvm_int64(0), wait_ptr);
   LLVMBuildRetVoid(builder);

   // Sequential statements

   LLVMPositionBuilderAtEnd(builder, start_bb);

   for (int i = 0; i < nstmts; i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   LLVMBuildBr(builder, start_bb);

   // Free context memory

   while (ctx.entry_list != NULL) {
      proc_entry_t *next = ctx.entry_list->next;
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
   // This is passed a length and raw array of value which needs to be
   // wrapped in meta data

   char *name = xasprintf("%s$resolution", istr(type_ident(type)));

   LLVMValueRef fn = LLVMGetNamedFunction(module, name);
   if (fn != NULL) {
      free(name);
      return fn;    // Already generated wrapper
   }

   LLVMTypeRef elem_type = llvm_type(type);

   LLVMTypeRef args[] = {
      LLVMPointerType(elem_type, 0),
      LLVMInt32Type()
   };

   fn = LLVMAddFunction(module, name,
                        LLVMFunctionType(LLVMInt64Type(), args,
                                         ARRAY_LEN(args), false));
   free(name);

   LLVMBasicBlockRef saved_bb = LLVMGetInsertBlock(builder);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   // Wrap array in meta data and call actual resolution function

   tree_t fdecl = tree_ref(type_resolution(type));
   type_t ftype = tree_type(fdecl);

   // TODO: check what standard says about left/right and direction
   LLVMValueRef left  = llvm_int32(0);
   LLVMValueRef right = LLVMBuildSub(builder, LLVMGetParam(fn, 1),
                                     llvm_int32(1), "right");
   LLVMValueRef dir   = llvm_int8(RANGE_TO);
   LLVMValueRef vals  = LLVMGetParam(fn, 0);

   LLVMValueRef wrapped =
      cgen_array_meta_1(type_param(ftype, 0), left, right, dir, vals);

   const char *rfn_name = cgen_mangle_func_name(fdecl);
   LLVMValueRef rfn = LLVMGetNamedFunction(module, rfn_name);
   if (rfn == NULL) {
      // The resolution function is not visible yet e.g. because it
      // is declared in another package
      LLVMTypeRef args[tree_ports(fdecl)];
      unsigned nargs;
      cgen_prototype(fdecl, args, &nargs, false, NULL);

      rfn = LLVMAddFunction(module, rfn_name,
                            LLVMFunctionType(elem_type, args, nargs, false));
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

   LLVMTypeRef  nid_type = cgen_net_id_type();
   LLVMValueRef map_var = NULL;

   const int nnets = tree_nets(t);
   if (nnets == 0) {
      if (tree_attr_int(t, null_range_i, 0)) {
         // Special case of array signal with null range
         char *buf = xasprintf("%s_nets", istr(tree_ident(t)));

         LLVMTypeRef map_type = LLVMArrayType(nid_type, 0);
         map_var = LLVMAddGlobal(module, map_type, buf);
         LLVMSetInitializer(map_var, LLVMGetUndef(map_type));
         free(buf);
      }
      else {
         // Not an elaborated design
         char *buf = xasprintf("%s_nets",
                               package_signal_path_name(tree_ident(t)));

         LLVMTypeRef map_type = LLVMArrayType(nid_type, 0);
         map_var = LLVMAddGlobal(module, map_type, buf);
         LLVMSetLinkage(map_var, LLVMExternalLinkage);
         free(buf);
      }
   }
   else {
      char *buf = xasprintf("%s_nets", istr(tree_ident(t)));

      LLVMTypeRef map_type = LLVMArrayType(nid_type, nnets);
      map_var = LLVMAddGlobal(module, map_type, buf);
      free(buf);

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
   }

   tree_add_attr_ptr(t, sig_nets_i, map_var);
}

static void cgen_subprogram_locals(tree_t t, cgen_ctx_t *ctx)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_CONST_DECL:
         {
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
               break;
            }
            // Fall-through
         }
      case T_VAR_DECL:
         {
            LLVMValueRef var = cgen_local_var(d, ctx);
            tree_add_attr_ptr(d, local_var_i, var);
         }
         break;
      default:
         break;
      }
   }
}

static void cgen_func_body(tree_t t, tree_t parent)
{
   cgen_nested_subprograms(t);

   LLVMValueRef fn = cgen_fdecl(t, parent);
   const unsigned nargs = LLVMCountParams(fn);

   LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(fn, "entry");
   LLVMPositionBuilderAtEnd(builder, entry_bb);

   cgen_ctx_t ctx = {
      .entry_list = NULL,
      .proc       = NULL,
      .fdecl      = t,
      .fn         = fn
   };

   if (tree_attr_int(t, nest_level_i, 0))
      ctx.nest_state = LLVMGetParam(fn, nargs - 1);

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
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

   cgen_subprogram_locals(t, &ctx);

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   LLVMBuildUnreachable(builder);
}

static void cgen_proc_body(tree_t t, tree_t parent)
{
   // Procedures take an extra "context" parameter which is used to support
   // suspending and resuming. If the procedure returns non-NULL then this
   // pointer should be saved, the caller should suspend, and when it
   // resumes call the procedure again with the saved pointer as the first
   // argument. If the procedure returns NULL execution continues as
   // normal.

   cgen_nested_subprograms(t);

   const int nports = tree_ports(t);
   unsigned nargs;
   LLVMTypeRef args[nports + 2];
   cgen_prototype(t, args, &nargs, true, parent);

   const char *mangled = cgen_mangle_func_name(t);
   LLVMValueRef fn = LLVMGetNamedFunction(module, mangled);
   if (fn == NULL) {
      fn = LLVMAddFunction(module, mangled,
                           LLVMFunctionType(llvm_void_ptr(),
                                            args, nargs, false));
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

   cgen_ctx_t ctx = {
      .entry_list = NULL,
      .proc       = NULL,
      .fdecl      = t,
      .fn         = fn,
      .state      = NULL
   };

   if (tree_attr_int(t, nest_level_i, 0))
      ctx.nest_state = LLVMGetParam(fn, nargs - 2);

   // Generate a jump table to handle resuming from a wait statement

   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      tree_visit(tree_stmt(t, i), cgen_jump_table_fn, &ctx);

   if (ctx.entry_list != NULL) {
      // Only allocate a dynamic context if there are no wait statements

      unsigned nfields;
      LLVMTypeRef *fields = cgen_state_type_fields(t, &nfields);

      LLVMTypeRef state_type = LLVMStructType(fields, nfields, false);
      LLVMTypeRef state_ptr_type = LLVMPointerType(state_type, 0);

      free(fields);

      LLVMBasicBlockRef init_bb   = LLVMAppendBasicBlock(fn, "init");
      LLVMBasicBlockRef resume_bb = LLVMAppendBasicBlock(fn, "resume");
      LLVMBasicBlockRef start_bb  = LLVMAppendBasicBlock(fn, "start");
      LLVMBasicBlockRef jump_bb   = LLVMAppendBasicBlock(fn, "jump");

      // We are resuming the procedure if the context is non-NULL
      LLVMValueRef resume = LLVMBuildICmp(
         builder, LLVMIntNE, LLVMGetParam(fn, nargs - 1),
         LLVMConstNull(llvm_void_ptr()), "resume");

      LLVMBuildCondBr(builder, resume, resume_bb, init_bb);

      // When resuming get the state from the final argument

      LLVMPositionBuilderAtEnd(builder, resume_bb);

      LLVMValueRef old = LLVMBuildPointerCast(
         builder, LLVMGetParam(fn, nargs - 1),
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
      cgen_subprogram_locals(t, &ctx);

   for (int i = 0; i < nstmts; i++)
      cgen_stmt(tree_stmt(t, i), &ctx);

   if (ctx.state != NULL) {
      // Free any dynamically allocated arrays
      const int ndecls = tree_decls(t);
      for (int i = 0; i < ndecls; i++) {
         tree_t decl = tree_decl(t, i);
         tree_kind_t kind = tree_kind(decl);
         if ((kind != T_VAR_DECL) && (kind != T_CONST_DECL))
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
   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      type_t type = tree_type(value);

      if (type_is_array(type) && cgen_is_const(value)) {
         // The aggregate generation code will add a global variable
         // that we can reuse here
         LLVMValueRef llvalue = cgen_expr(value, NULL);
         LLVMSetValueName(llvalue, istr(tree_ident(t)));
         LLVMSetLinkage(llvalue, LLVMExternalLinkage);
         tree_add_attr_ptr(t, local_var_i, llvalue);
      }
      else {
         LLVMTypeRef lltype = llvm_type(tree_type(t));
         LLVMValueRef v = LLVMAddGlobal(module, lltype, istr(tree_ident(t)));

         tree_t value = tree_value(t);
         if (cgen_is_const(value)) {
            // Value is a compile-time constant
            LLVMValueRef init = cgen_expr(value, NULL);
            LLVMSetInitializer(v, init);
            LLVMSetGlobalConstant(v, true);
            tree_add_attr_ptr(t, local_var_i, v);
         }
         else {
            // The value will be generated by the reset function
            LLVMSetInitializer(v, LLVMGetUndef(lltype));
            tree_add_attr_ptr(t, global_const_i, v);
         }
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
   char *name = xasprintf("%s_reset", istr(tree_ident(t)));
   LLVMValueRef fn =
      LLVMAddFunction(module, name,
                      LLVMFunctionType(LLVMVoidType(), NULL, 0, false));
   free(name);

   cgen_ctx_t ctx = {
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
      if (nnets == 0)
         continue;

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
      tree_t value = tree_value(d);
      type_t init_type = tree_type(value);
      LLVMValueRef val = cgen_expr(value, &ctx);

      type_t decl_type = tree_type(d);

      LLVMValueRef n_elems, size;
      if (!type_is_array(init_type)) {
         // Need to get a pointer to the data
         LLVMTypeRef lltype = LLVMTypeOf(val);
         LLVMValueRef tmp = LLVMBuildAlloca(builder, lltype, "");
         LLVMBuildStore(builder, val, tmp);
         val = tmp;

         n_elems = llvm_int32(1);
         size    = llvm_sizeof(lltype);
      }
      else {
         cgen_check_array_sizes(value, decl_type, init_type,
                                NULL, val, &ctx);

         val     = cgen_array_data_ptr(init_type, val);
         n_elems = cgen_array_len_recur(decl_type, val);
         size    = cgen_array_elem_size(decl_type);
      }

      // Assuming array nets are sequential
      netid_t nid = tree_net(d, 0);

      LLVMValueRef args[] = {
         llvm_int32(nid),
         llvm_void_cast(val),
         n_elems,
         size,
         llvm_void_cast(cgen_resolution_func(decl_type)),
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

   LLVMValueRef cover_conds = LLVMGetNamedGlobal(module, "cover_conds");
   if (cover_stmts != NULL) {
      LLVMValueRef memset_args[] = {
         llvm_void_cast(cover_conds),
         llvm_int8(0),
         llvm_int32(tree_attr_int(t, ident_new("stmt_conds"), 0) * 4),
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
   type_t type  = tree_type(t);
   tree_t value = tree_value(t);

   LLVMValueRef var;
   if (type_is_array(type)) {
      var = cgen_expr(value, NULL);
      LLVMSetValueName(var, istr(tree_ident(t)));
      LLVMSetLinkage(var, LLVMExternalLinkage);
      LLVMSetGlobalConstant(var, false);
   }
   else {
      var = LLVMAddGlobal(module, llvm_type(type), istr(tree_ident(t)));

      LLVMValueRef init = cgen_expr(value, NULL);
      LLVMSetInitializer(var, init);
   }

   tree_add_attr_ptr(t, local_var_i, var);
}

static void cgen_coverage_state(tree_t t)
{
   const int stmt_tags = tree_attr_int(t, ident_new("stmt_tags"), 0);
   if (stmt_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_stmts");
      LLVMSetInitializer(var, LLVMGetUndef(type));
   }

   const int cond_tags = tree_attr_int(t, ident_new("cond_tags"), 0);
   if (cond_tags > 0) {
      LLVMTypeRef type = LLVMArrayType(LLVMInt32Type(), stmt_tags);
      LLVMValueRef var = LLVMAddGlobal(module, type, "cover_conds");
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
         cgen_func_body(decl, NULL);
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
         cgen_proc_body(decl, NULL);
         break;
      case T_FILE_DECL:
         cgen_file_decl(decl);
         break;
      case T_VAR_DECL:
         cgen_shared_var(decl);
         break;
      case T_ATTR_DECL:
      case T_ATTR_SPEC:
      case T_HIER:
      case T_COMPONENT:
      case T_CONTEXT:
         break;
      default:
         fatal("cannot generate code for top level declaration %s",
               tree_kind_str(tree_kind(decl)));
      }
   }

   cgen_reset_function(t);

   if (tree_kind(t) == T_ELAB) {
      const int nstmts = tree_stmts(t);
      for (int i = 0; i < nstmts; i++)
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

static LLVMValueRef cgen_support_fn(const char *name)
{
   if (strcmp(name, "_sched_process") == 0) {
      LLVMTypeRef args[] = { LLVMInt64Type() };
      return LLVMAddFunction(module, "_sched_process",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_sched_waveform") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt64Type(),
         LLVMInt64Type()
      };
      return LLVMAddFunction(module, "_sched_waveform",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_sched_event") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_sched_event",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_set_initial") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      return LLVMAddFunction(module, "_set_initial",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_assert_fail") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      return LLVMAddFunction(module, "_assert_fail",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_vec_load") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      return LLVMAddFunction(module, "_vec_load",
                             LLVMFunctionType(llvm_void_ptr(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_image") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt64Type(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMPointerType(llvm_uarray_type(LLVMInt8Type(), 1), 0)
      };
      return LLVMAddFunction(module, "_image",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_debug_out") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_debug_out",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_debug_dump") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_debug_dump",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.pow.f64") == 0) {
      LLVMTypeRef args[] = {
         LLVMDoubleType(),
         LLVMDoubleType()
      };
      return LLVMAddFunction(module, "llvm.pow.f64",
                             LLVMFunctionType(LLVMDoubleType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "llvm.memset.p0i8.i32") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt8Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      return LLVMAddFunction(module, "llvm.memset.p0i8.i32",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strncmp(name, "llvm.memcpy", 11) == 0) {
      int width;
      if (sscanf(name, "llvm.memcpy.p0i%d", &width) != 1)
         fatal("invalid memcpy intrinsic %s", name);

      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMPointerType(LLVMIntType(width), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt1Type()
      };
      return LLVMAddFunction(module, cgen_memcpy_name(width),
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_open") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMPointerType(llvm_void_ptr(), 0),
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type()
      };
      return LLVMAddFunction(module, "_file_open",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_write") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0),
         llvm_void_ptr(),
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_file_write",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_read") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0),
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt32Type(), 0)
      };
      return LLVMAddFunction(module, "_file_read",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_file_close") == 0) {
      LLVMTypeRef args[] = {
         LLVMPointerType(llvm_void_ptr(), 0)
      };
      return LLVMAddFunction(module, "_file_close",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_endfile") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr()
      };
      return LLVMAddFunction(module, "_endfile",
                             LLVMFunctionType(LLVMInt1Type(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_bounds_fail") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_bounds_fail",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_div_zero") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      return LLVMAddFunction(module, "_div_zero",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_null_deref") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      return LLVMAddFunction(module, "_null_deref",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_bit_shift") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMInt32Type(),
         LLVMPointerType(llvm_uarray_type(LLVMInt1Type(), 1), 0)
      };
      return LLVMAddFunction(module, "_bit_shift",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_bit_vec_op") == 0) {
      LLVMTypeRef args[] = {
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMPointerType(LLVMInt1Type(), 0),
         LLVMInt32Type(),
         LLVMInt8Type(),
         LLVMPointerType(llvm_uarray_type(LLVMInt1Type(), 1), 0)
      };
      return LLVMAddFunction(module, "_bit_vec_op",
                             LLVMFunctionType(LLVMVoidType(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_test_net_flag") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_test_net_flag",
                             LLVMFunctionType(LLVMInt1Type(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_last_event") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type()
      };
      return LLVMAddFunction(module, "_last_event",
                             LLVMFunctionType(LLVMInt64Type(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_value_attr") == 0) {
      LLVMTypeRef args[] = {
         llvm_void_ptr(),
         LLVMInt32Type(),
         LLVMInt32Type(),
         LLVMPointerType(LLVMInt8Type(), 0)
      };
      return LLVMAddFunction(module, "_value_attr",
                             LLVMFunctionType(LLVMInt64Type(),
                                              args, ARRAY_LEN(args), false));
   }
   else if (strcmp(name, "_std_standard_now") == 0)
      return LLVMAddFunction(module, "_std_standard_now",
                             LLVMFunctionType(LLVMInt64Type(), NULL, 0, false));
   else
      return NULL;
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

static void cgen_tmp_stack(void)
{
   LLVMValueRef _tmp_stack =
      LLVMAddGlobal(module, LLVMPointerType(llvm_void_ptr(), 0), "_tmp_stack");
   LLVMSetLinkage(_tmp_stack, LLVMExternalLinkage);

   LLVMValueRef _tmp_alloc =
      LLVMAddGlobal(module, LLVMInt32Type(), "_tmp_alloc");
   LLVMSetLinkage(_tmp_alloc, LLVMExternalLinkage);
}

static void cgen_cleanup_tmp_attrs(tree_t top)
{
   // Delete any LLVM pointers stored as tree attributes

   const int ndecls = tree_decls(top);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(top, i);

      if (tree_attr_ptr(d, local_var_i))
         tree_add_attr_ptr(d, local_var_i, NULL);

      if (tree_attr_ptr(d, global_const_i))
         tree_add_attr_ptr(d, global_const_i, NULL);

      if (tree_attr_ptr(d, sig_nets_i))
         tree_add_attr_ptr(d, sig_nets_i, NULL);
   }
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
   cond_tag_i     = ident_new("cond_tag");
   elide_bounds_i = ident_new("elide_bounds");
   null_range_i   = ident_new("null_range");
   nest_level_i   = ident_new("nest_level");
   nest_offset_i  = ident_new("nest_offset");
   nest_parent_i  = ident_new("nest_parent");
   sub_cond_i     = ident_new("sub_cond");
   returned_i     = ident_new("returned");
   static_i       = ident_new("static");

   tree_kind_t kind = tree_kind(top);
   if ((kind != T_ELAB) && (kind != T_PACK_BODY) && (kind != T_PACKAGE))
      fatal("cannot generate code for %s", tree_kind_str(kind));

   module = LLVMModuleCreateWithName(istr(tree_ident(top)));
   builder = LLVMCreateBuilder();

   cgen_module_name(top);
   cgen_tmp_stack();

   cgen_top(top);

   if (opt_get_int("dump-llvm"))
      LLVMDumpModule(module);

   if (LLVMVerifyModule(module, LLVMPrintMessageAction, NULL))
      fatal("LLVM verification failed");

   optimise();

   char *fname = xasprintf("_%s.bc", istr(tree_ident(top)));

   FILE *f = lib_fopen(lib_work(), fname, "w");
   if (LLVMWriteBitcodeToFD(module, fileno(f), 0, 0) != 0)
      fatal("error writing LLVM bitcode");
   fclose(f);
   free(fname);

   cgen_cleanup_tmp_attrs(top);

   LLVMDisposeBuilder(builder);
   LLVMDisposeModule(module);
}
