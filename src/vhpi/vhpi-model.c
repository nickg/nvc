//
//  Copyright (C) 2014-2026  Nick Gasson
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
#include "array.h"
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "jit/jit-ffi.h"
#include "jit/jit-layout.h"
#include "jit/jit.h"
#include "lib.h"
#include "option.h"
#include "printf.h"
#include "rt/model.h"
#include "rt/rt.h"
#include "rt/structs.h"
#include "tree.h"
#include "type.h"
#include "vhpi/vhpi-macros.h"
#include "vhpi/vhpi-model.h"
#include "vhpi/vhpi-priv.h"

#include <assert.h>
#include <math.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef vhpiCharT      *vhpiStringT;
typedef vhpiSmallEnumT  vhpiBooleanT;

typedef struct {
   tree_t         tree;
   vhpiClassKindT kind;
} c_vhpiObject;

typedef struct {
   c_vhpiObject object;
   uint32_t     refcount;
} c_refcounted;

typedef struct {
   unsigned       count;
   unsigned       limit;
   c_vhpiObject **items;
} vhpiObjectListT;

typedef A(vhpiHandleT) vhpiHandleListT;

typedef void (*vhpiLazyFnT)(c_vhpiObject *);

typedef struct {
   vhpiLazyFnT     fn;
   vhpiObjectListT list;
} vhpiLazyListT;

typedef struct {
   c_vhpiObject object;
   vhpiStringT  StrVal;
} c_argv;

DEF_CLASS(argv, vhpiArgvK, object);

typedef struct {
   c_vhpiObject      object;
   vhpiObjectListT   argv;
   vhpiCapabibilityT capabilities;
} c_tool;

DEF_CLASS(tool, vhpiToolK, object);

typedef struct tag_abstractRegion c_abstractRegion;
typedef struct tag_expr c_expr;

typedef struct tag_abstractRegion {
   c_vhpiObject      object;
   jit_handle_t      handle;
   UNSAFE_MPTR       privdata;
   vhpiLazyListT     Decls;
   vhpiLazyListT     Stmts;
   c_abstractRegion *UpperRegion;
   vhpiStringT       Name;
   vhpiStringT       FullName;
} c_abstractRegion;

typedef struct {
   c_vhpiObject      object;
   c_abstractRegion *ImmRegion;
   vhpiStringT       Name;
   vhpiStringT       FullName;
} c_abstractDecl;

typedef struct {
   c_vhpiObject  object;
   c_expr       *LeftExpr;
   c_expr       *RightExpr;
   vhpiBooleanT  Staticness;
   vhpiBooleanT  IsUnconstrained;
   vhpiBooleanT  IsNull;
   vhpiBooleanT  IsUp;
   vhpiBooleanT  IsDiscrete;
} c_range;

typedef struct {
   c_range   range;
   vhpiPhysT PhysLeftBound;
   vhpiPhysT PhysRightBound;
} c_physRange;

DEF_CLASS(physRange, vhpiPhysRangeK, range.object);

typedef struct {
   c_range       range;
   c_vhpiObject *source;
   unsigned      nth_dim;
   vhpiLongIntT  LeftBound;
   vhpiLongIntT  RightBound;
} c_intRange;

DEF_CLASS(intRange, vhpiIntRangeK, range.object);

typedef struct {
   c_range   range;
   vhpiRealT FloatLeftBound;
   vhpiRealT FloatRightBound;
} c_floatRange;

DEF_CLASS(floatRange, vhpiFloatRangeK, range.object);

typedef struct tag_typeDecl c_typeDecl;

typedef struct tag_typeDecl {
   c_abstractDecl  decl;
   type_t          type;
   ptrdiff_t       offset;
   vhpiFormatT     format;
   bool            homogeneous;
   uint8_t         elem_bytes;
   const char     *map_str;
   vhpiBooleanT    IsComposite;
   vhpiBooleanT    IsScalar;
   vhpiBooleanT    IsUnconstrained;
} c_typeDecl;

typedef struct {
   c_typeDecl      typeDecl;
   c_typeDecl     *BaseType;
   c_typeDecl     *ElemType;  // TODO: elem constraint?
   vhpiLazyListT   Constraints;
   c_vhpiObject   *source;
   unsigned        depth;     // For nested arrays
   vhpiIntT        Size;
   vhpiBooleanT    IsAnonymous;
} c_subTypeDecl;

DEF_CLASS(subTypeDecl, vhpiSubtypeDeclK, typeDecl.decl.object);

typedef struct {
   c_typeDecl typeDecl;
} c_scalarTypeDecl;

typedef struct {
   c_typeDecl typeDecl;
} c_compositeTypeDecl;

typedef struct {
   c_scalarTypeDecl scalar;
   c_range         *constraint;
} c_intTypeDecl;

DEF_CLASS(intTypeDecl, vhpiIntTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_scalarTypeDecl scalar;
   vhpiLazyListT    EnumLiterals;
} c_enumTypeDecl;

DEF_CLASS(enumTypeDecl, vhpiEnumTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_scalarTypeDecl scalar;
   c_range         *constraint;
   vhpiLazyListT    UnitDecls;
} c_physTypeDecl;

DEF_CLASS(physTypeDecl, vhpiPhysTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_scalarTypeDecl scalar;
   c_range         *constraint;
} c_floatTypeDecl;

DEF_CLASS(floatTypeDecl, vhpiFloatTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_typeDecl  typeDecl;
   c_typeDecl *ValType;
} c_accessTypeDecl;

DEF_CLASS(accessTypeDecl, vhpiAccessTypeDeclK, typeDecl.decl.object);

typedef struct {
   c_typeDecl  typeDecl;
   c_typeDecl *ValType;
} c_fileTypeDecl;

DEF_CLASS(fileTypeDecl, vhpiFileTypeDeclK, typeDecl.decl.object);

typedef struct {
   c_compositeTypeDecl composite;
   vhpiIntT            NumDimensions;
   c_typeDecl         *ElemType;
} c_arrayTypeDecl;

DEF_CLASS(arrayTypeDecl, vhpiArrayTypeDeclK, composite.typeDecl.decl.object);

typedef struct {
   c_compositeTypeDecl composite;
   vhpiLazyListT       RecordElems;
} c_recordTypeDecl;

DEF_CLASS(recordTypeDecl, vhpiRecordTypeDeclK, composite.typeDecl.decl.object);

typedef struct {
   c_abstractDecl  decl;
   c_abstractDecl *parent;
   c_typeDecl     *Type;
   vhpiIntT        Position;
} c_elemDecl;

DEF_CLASS(elemDecl, vhpiElemDeclK, decl.object);

typedef struct {
   c_abstractDecl   decl;
   ptrdiff_t        offset;
   c_typeDecl      *Type;
   vhpiLazyListT    IndexedNames;
   vhpiLazyListT    SelectedNames;
   vhpiStaticnessT  Staticness;
} c_objDecl;

typedef struct {
   c_objDecl    objDecl;
   vhpiSigKindT SigKind;
   vhpiBooleanT IsGuarded;
} c_sigDecl;

DEF_CLASS(sigDecl, vhpiSigDeclK, objDecl.decl.object);

typedef struct {
   c_objDecl    objDecl;
   vhpiSigKindT SigKind;
   vhpiBooleanT IsProtectedType;
   vhpiBooleanT IsShared;
} c_varDecl;

DEF_CLASS(varDecl, vhpiVarDeclK, objDecl.decl.object);

typedef struct {
   c_abstractDecl  decl;
   vhpiObjectListT Params;
} c_subpDecl;

typedef struct {
   c_subpDecl    subpDecl;
   vhpiIntT      size;
   c_typeDecl   *ReturnType;
   vhpiBooleanT  IsPure;
} c_funcDecl;

DEF_CLASS(funcDecl, vhpiFuncDeclK, subpDecl.decl.object);

typedef struct {
   c_subpDecl subpDecl;
} c_procDecl;

DEF_CLASS(procDecl, vhpiProcDeclK, subpDecl.decl.object);

typedef struct {
   c_objDecl   objDecl;
   vhpiIntT    Position;
} c_interfaceDecl;

typedef struct {
   c_interfaceDecl interface;
   vhpiModeT       Mode;
   vhpiBooleanT    IsLocal;
   vhpiBooleanT    IsOpen;
   vhpiSigKindT    SigKind;
} c_portDecl;

DEF_CLASS(portDecl, vhpiPortDeclK, interface.objDecl.decl.object);

typedef struct {
   c_interfaceDecl interface;
   vhpiModeT       Mode;
   vhpiBooleanT    IsLocal;
   vhpiBooleanT    IsVital;
} c_genericDecl;

DEF_CLASS(genericDecl, vhpiGenericDeclK, interface.objDecl.decl.object);

typedef struct {
   c_interfaceDecl interface;
   unsigned        argslot;
   bool            wrapped;
} c_paramDecl;

typedef struct {
   c_paramDecl param;
   vhpiModeT   Mode;
} c_constParamDecl;

DEF_CLASS(constParamDecl, vhpiConstParamDeclK,
          param.interface.objDecl.decl.object);

typedef struct {
   c_paramDecl param;
   vhpiModeT   Mode;
} c_varParamDecl;

DEF_CLASS(varParamDecl, vhpiVarParamDeclK, param.interface.objDecl.decl.object);

typedef struct {
   c_objDecl    objDecl;
   vhpiBooleanT IsDeferred;
} c_constDecl;

DEF_CLASS(constDecl, vhpiConstDeclK, objDecl.decl.object);

typedef struct {
   c_abstractDecl decl;
   vhpiIntT       Position;
} c_enumLiteral;

DEF_CLASS(enumLiteral, vhpiEnumLiteralK, decl.object);

typedef struct {
   c_abstractDecl  decl;
   c_physTypeDecl *Type;
   vhpiPhysT       PhysPosition;
} c_unitDecl;

DEF_CLASS(unitDecl, vhpiUnitDeclK, decl.object);

typedef struct tag_expr {
   c_vhpiObject     object;
   c_typeDecl      *Type;
   vhpiStaticnessT  Staticness;
} c_expr;

typedef struct {
   c_expr expr;
} c_literal;

typedef struct {
   c_literal    literal;
   vhpiLongIntT IntVal;
} c_intLiteral;

typedef struct {
   c_literal literal;
   vhpiPhysT PhysVal;
} c_physLiteral;

typedef struct {
   c_expr        expr;
   vhpiLazyListT IndexedNames;
   vhpiLazyListT SelectedNames;
   vhpiStringT   FullName;
   vhpiStringT   Name;
   vhpiAccessT   Access;
} c_name;

typedef struct {
   c_name        name;
   c_vhpiObject *Prefix;
} c_prefixedName;

typedef struct {
   c_prefixedName prefixedName;
   vhpiIntT       BaseIndex;
   vhpiIntT       offset;
} c_indexedName;

DEF_CLASS(indexedName, vhpiIndexedNameK, prefixedName.name.expr.object);

typedef struct {
   c_prefixedName  prefixedName;
   c_elemDecl     *Suffix;
} c_selectedName;

DEF_CLASS(selectedName, vhpiSelectedNameK, prefixedName.name.expr.object);

typedef struct {
   c_abstractRegion region;
   vhpiStringT      UnitName;
} c_designUnit;

typedef struct {
   c_designUnit    designUnit;
   c_designUnit   *PrimaryUnit;
} c_secondaryUnit;

DEF_CLASS(secondaryUnit, vhpiArchBodyK, designUnit.region.object);

typedef struct {
   c_designUnit    designUnit;
} c_entityDecl;

typedef struct {
   c_designUnit designUnit;
} c_packDecl;

typedef struct {
   c_designUnit designUnit;
} c_verilogModule;

typedef struct {
   c_abstractRegion  region;
   c_designUnit     *DesignUnit;
} c_designInstUnit;

typedef struct {
   c_designInstUnit designInstUnit;
} c_rootInst;

DEF_CLASS(rootInst, vhpiRootInstK, designInstUnit.region.object);

typedef struct {
   c_designInstUnit designInstUnit;
} c_packInst;

DEF_CLASS(packInst, vhpiPackInstK, designInstUnit.region.object);

typedef struct {
   vhpiBooleanT IsSeqStmt;
   vhpiStringT  LabelName;
} c_stmt;

typedef struct {
   c_designInstUnit designInstUnit;
   c_stmt           stmt;
} c_compInstStmt;

DEF_CLASS(compInstStmt, vhpiCompInstStmtK, designInstUnit.region.object);

typedef struct {
   c_abstractRegion region;
   c_stmt           stmt;
   vhpiBooleanT     IsGuarded;
} c_blockStmt;

DEF_CLASS(blockStmt, vhpiBlockStmtK, region.object);

typedef struct {
   c_abstractRegion region;
   c_stmt           stmt;
   vhpiIntT         GenerateIndex;
   c_constDecl     *ParamDecl;
} c_forGenerate;

DEF_CLASS(forGenerate, vhpiForGenerateK, region.object);

typedef struct {
   c_abstractRegion region;
   c_stmt           stmt;
} c_ifGenerate;

DEF_CLASS(ifGenerate, vhpiIfGenerateK, region.object);

typedef struct {
   c_refcounted  refcounted;
   vhpiStateT    State;
   vhpiEnumT     Reason;
   vhpiCbDataT   data;
   vhpiHandleT   handle;
   rt_watch_t   *watch;
} c_callback;

DEF_CLASS(callback, vhpiCallbackK, refcounted.object);

typedef void *(*vhpiFilterT)(c_vhpiObject *);

typedef struct {
   c_refcounted     refcounted;
   vhpiObjectListT *list;
   c_vhpiObject    *single;
   uint32_t         pos;
   vhpiFilterT      filter;
} c_iterator;

DEF_CLASS(iterator, vhpiIteratorK, refcounted.object);

typedef struct {
   c_vhpiObject      object;
   vhpiForeignDataT  data;
   c_subpDecl       *decl;
   vhpiHandleT       handle;
} c_foreignf;

DEF_CLASS(foreignf, vhpiForeignfK, object);

#define HANDLE_BITS      (sizeof(vhpiHandleT) * 8)
#define HANDLE_MAX_INDEX ((UINT64_C(1) << (HANDLE_BITS / 2)) - 1)

typedef enum {
   HANDLE_USER,
   HANDLE_INTERNAL,
} handle_kind_t;

typedef struct {
   c_vhpiObject *obj;
   handle_kind_t kind;
   uint32_t      generation;
} handle_slot_t;

STATIC_ASSERT(sizeof(handle_slot_t) <= 16);

typedef struct _vhpi_context {
   c_tool          *tool;
   c_rootInst      *root;
   vhpiObjectListT  packages;
   shash_t         *strtab;
   rt_model_t      *model;
   hash_t          *objcache;
   tree_t           top;
   jit_t           *jit;
   handle_slot_t   *handles;
   unsigned         num_handles;
   unsigned         free_hint;
   vhpiObjectListT  foreignfs;
   jit_scalar_t    *args;
   tlab_t          *tlab;
   vhpiHandleListT  callbacks;
   mem_pool_t      *pool;
   vhpiObjectListT  recycle;
   vhpiPhaseT       phase;
} vhpi_context_t;

typedef enum {
   PTR_NULL,
   PTR_RAW,
   PTR_SIGNAL,
} ptr_kind_t;

typedef struct {
   ptr_kind_t   kind;
   uint32_t     offset;
   void        *data;
   rt_signal_t *signal;
   ffi_dim_t   *bounds;
} vhpi_ptr_t;

typedef struct {
   rt_signal_t *signal;
   uint32_t     offset;
   uint32_t     count;
} vhpi_slice_t;

typedef A(vhpi_slice_t) slice_list_t;

static c_designUnit *cached_designUnit(tree_t t);
static vhpiStringT vhpi_get_name(c_vhpiObject *obj);
static vhpiStringT vhpi_get_full_name(c_vhpiObject *obj);
static vhpi_ptr_t vhpi_get_ptr(c_vhpiObject *obj);
static c_typeDecl *vhpi_get_type(c_vhpiObject *obj);
static vhpiIntT vhpi_get_size(c_vhpiObject *obj);
static const jit_layout_t *vhpi_get_layout(c_vhpiObject *obj);
static void vhpi_lazy_decls(c_vhpiObject *obj);
static void vhpi_lazy_stmts(c_vhpiObject *obj);
static void vhpi_lazy_selected_names(c_vhpiObject *obj);
static void vhpi_lazy_indexed_names(c_vhpiObject *obj);
static void vhpi_lazy_enum_literals(c_vhpiObject *obj);
static void vhpi_lazy_unit_decls(c_vhpiObject *obj);
static void vhpi_lazy_fields(c_vhpiObject *obj);
static void vhpi_lazy_constraints(c_vhpiObject *obj);
static const char *handle_pp(vhpiHandleT handle);
static c_refcounted *is_refcounted(c_vhpiObject *obj);
static c_tool *build_tool(int argc, char **argv);

static vhpi_context_t *global_context = NULL;   // TODO: thread local

static const vhpiModeT mode_map[] = {
   vhpiInMode, vhpiInMode, vhpiOutMode, vhpiInoutMode, vhpiBufferMode,
   vhpiLinkageMode, vhpiLinkageMode /* view */, vhpiLinkageMode /* view */
};

static inline vhpi_context_t *vhpi_context(void)
{
   assert(global_context != NULL);
   return global_context;
}

static handle_slot_t *decode_handle(vhpi_context_t *c, vhpiHandleT handle)
{
   const uintptr_t bits = (uintptr_t)handle;
   const uint32_t index = bits & HANDLE_MAX_INDEX;
   const uint32_t generation = bits >> HANDLE_BITS/2;

   if (handle == NULL || index >= c->num_handles)
      return NULL;

   handle_slot_t *slot = &(c->handles[index]);
   if (slot->obj == NULL)
      return NULL;
   else if (slot->generation != generation)
      return NULL;   // Use-after-free

   return slot;
}

static inline vhpiHandleT encode_handle(handle_slot_t *slot, uint32_t index)
{
   const uintptr_t bits = (uintptr_t)slot->generation << HANDLE_BITS/2 | index;
   return (vhpiHandleT)bits;
}

static vhpiHandleT handle_for(c_vhpiObject *obj, handle_kind_t kind)
{
   assert(obj != NULL);

   vhpi_context_t *c = vhpi_context();

   uint32_t index = c->free_hint;
   if (index >= c->num_handles || c->handles[index].obj != NULL) {
      for (index = 0; index < c->num_handles; index++) {
         if (c->handles[index].obj == NULL
             && c->handles[index].generation < HANDLE_MAX_INDEX)
            break;
      }
   }

   if (unlikely(index > HANDLE_MAX_INDEX)) {
      vhpi_error(vhpiFailure, NULL, "too many active handles");
      return NULL;
   }
   else if (index == c->num_handles) {
      const int new_size = MAX(c->num_handles * 2, 128);
      c->handles = xrealloc_array(c->handles, new_size, sizeof(handle_slot_t));
      c->num_handles = new_size;

      for (int i = index; i < new_size; i++) {
         c->handles[i].obj = NULL;
         c->handles[i].generation = 1;
      }
   }

   handle_slot_t *slot = &(c->handles[index]);
   slot->obj  = obj;
   slot->kind = kind;

   c->free_hint = index + 1;

   c_refcounted *rc = is_refcounted(obj);
   if (rc != NULL)
      rc->refcount++;

   return encode_handle(slot, index);
}

static inline vhpiHandleT user_handle_for(c_vhpiObject *obj)
{
   return handle_for(obj, HANDLE_USER);
}

static inline vhpiHandleT internal_handle_for(c_vhpiObject *obj)
{
   return handle_for(obj, HANDLE_INTERNAL);
}

static void drop_handle(vhpi_context_t *c, vhpiHandleT handle)
{
   handle_slot_t *slot = decode_handle(c, handle);
   if (slot == NULL)
      return;

   c_vhpiObject *obj = slot->obj;
   slot->obj = NULL;
   slot->generation++;

   c->free_hint = slot - c->handles;

   c_refcounted *rc = is_refcounted(obj);
   if (rc != NULL) {
      assert(rc->refcount > 0);
      if (--(rc->refcount) == 0)
         APUSH(c->recycle, obj);
   }
}

static inline c_vhpiObject *from_handle(vhpiHandleT handle)
{
   handle_slot_t *slot = decode_handle(vhpi_context(), handle);
   if (likely(slot != NULL))
      return slot->obj;

   vhpi_error(vhpiError, NULL, "invalid handle %p", handle);
   return NULL;
}

static const loc_t *obj_loc(c_vhpiObject *obj)
{
   if (obj != NULL && obj->tree != NULL)
      return tree_loc(obj->tree);
   else
      return NULL;
}

static c_refcounted *is_refcounted(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiCallbackK:
   case vhpiIteratorK:
      return container_of(obj, c_refcounted, object);
   default:
      return NULL;
   }
}

static c_abstractRegion *is_abstractRegion(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiBlockStmtK:
   case vhpiCompInstStmtK:
   case vhpiForGenerateK:
   case vhpiIfGenerateK:
   case vhpiPackInstK:
   case vhpiEntityDeclK:
   case vhpiArchBodyK:
   case vhpiPackDeclK:
      return container_of(obj, c_abstractRegion, object);
   default:
      return NULL;
   }
}

static c_abstractDecl *is_abstractDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigDeclK:
   case vhpiPortDeclK:
   case vhpiEnumLiteralK:
   case vhpiIntTypeDeclK:
   case vhpiEnumTypeDeclK:
   case vhpiPhysTypeDeclK:
   case vhpiFloatTypeDeclK:
   case vhpiArrayTypeDeclK:
   case vhpiRecordTypeDeclK:
   case vhpiAccessTypeDeclK:
   case vhpiFileTypeDeclK:
   case vhpiSubtypeDeclK:
   case vhpiElemDeclK:
   case vhpiGenericDeclK:
   case vhpiConstDeclK:
   case vhpiConstParamDeclK:
   case vhpiSigParamDeclK:
   case vhpiVarParamDeclK:
      return container_of(obj, c_abstractDecl, object);
   default:
      return NULL;
   }
}

static c_objDecl *is_objDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigDeclK:
   case vhpiPortDeclK:
   case vhpiGenericDeclK:
   case vhpiConstDeclK:
   case vhpiConstParamDeclK:
   case vhpiVarParamDeclK:
   case vhpiSigParamDeclK:
      return container_of(obj, c_objDecl, decl.object);
   default:
      return NULL;
   }
}

static c_objDecl *cast_objDecl(c_vhpiObject *obj)
{
   c_objDecl *od = is_objDecl(obj);
   if (od == NULL)
      vhpi_error(vhpiError, obj_loc(obj), "class kind %s is not an object "
                 "declaration", vhpi_class_str(obj->kind));
   return od;
}

static c_paramDecl *is_paramDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigParamDeclK:
   case vhpiVarParamDeclK:
   case vhpiConstParamDeclK:
      return container_of(obj, c_paramDecl, interface.objDecl.decl.object);
   default:
      return NULL;
   }
}

static c_range *is_range(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiPhysRangeK:
   case vhpiIntRangeK:
      return container_of(obj, c_range, object);
   default:
      return NULL;
   }
}

static c_subpDecl *is_subpDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiFuncDeclK:
   case vhpiProcDeclK:
      return container_of(obj, c_subpDecl, decl.object);
   default:
      return NULL;
   }
}
static c_typeDecl *is_typeDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSubtypeDeclK:
   case vhpiIntTypeDeclK:
   case vhpiEnumTypeDeclK:
   case vhpiPhysTypeDeclK:
   case vhpiArrayTypeDeclK:
   case vhpiRecordTypeDeclK:
   case vhpiAccessTypeDeclK:
   case vhpiFileTypeDeclK:
   case vhpiFloatTypeDeclK:
      return container_of(obj, c_typeDecl, decl.object);
   default:
      return NULL;
   }
}

static c_expr *is_expr(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiIndexedNameK:
   case vhpiSelectedNameK:
      return container_of(obj, c_expr, object);
   default:
      return NULL;
   }
}

static c_expr *cast_expr(c_vhpiObject *obj)
{
   c_expr *e = is_expr(obj);
   if (e == NULL)
      vhpi_error(vhpiError, obj_loc(obj), "class kind %s is not an expression",
                 vhpi_class_str(obj->kind));

   return e;
}

static c_stmt *is_stmt(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiBlockStmtK:
      return &(container_of(obj, c_blockStmt, region.object)->stmt);
   case vhpiCompInstStmtK:
      return &(container_of(obj, c_compInstStmt,
                            designInstUnit.region.object)->stmt);
   case vhpiForGenerateK:
      return &(container_of(obj, c_forGenerate, region.object)->stmt);
   case vhpiIfGenerateK:
      return &(container_of(obj, c_ifGenerate, region.object)->stmt);
   default:
      return NULL;
   }
}

static c_name *is_name(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiIndexedNameK:
   case vhpiSelectedNameK:
      return container_of(obj, c_name, expr.object);
   default:
      return NULL;
   }
}

static c_prefixedName *is_prefixedName(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiIndexedNameK:
   case vhpiSelectedNameK:
      return container_of(obj, c_prefixedName, name.expr.object);
   default:
      return NULL;
   }
}

static c_designUnit *is_designUnit(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiArchBodyK:
   case vhpiEntityDeclK:
   case vhpiVerilogModuleK:
      return container_of(obj, c_designUnit, region.object);
   default:
      return NULL;
   }
}

static c_designInstUnit *cast_designInstUnit(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiCompInstStmtK:
   case vhpiPackInstK:
      return container_of(obj, c_designInstUnit, region.object);
   default:
      vhpi_error(vhpiError, obj_loc(obj), "class kind %s is not an instance of "
                 "a design unit", vhpi_class_str(obj->kind));
      return NULL;
   }
}

static void handle_pp_r(vhpiHandleT handle, text_buf_t *tb)
{
   if (handle == NULL) {
      tb_cat(tb, "NULL");
      return;
   }

   tb_printf(tb, "%p:{", handle);

   handle_slot_t *slot = decode_handle(vhpi_context(), handle);
   if (slot == NULL)
      tb_cat(tb, "INVALID");
   else {
      c_vhpiObject *obj = slot->obj;
      tb_cat(tb, vhpi_class_str(obj->kind));

      c_abstractDecl *decl = is_abstractDecl(obj);
      if (decl != NULL)
         tb_printf(tb, " Name=%s", vhpi_get_name(obj));

      c_name *n = is_name(obj);
      if (n != NULL)
         tb_printf(tb, " Name=%s", vhpi_get_name(obj));

      c_iterator *it = is_iterator(obj);
      if (it != NULL && it->list != NULL)
         tb_printf(tb, " pos=%d/%d", it->pos, it->list->count);

      c_callback *cb = is_callback(obj);
      if (cb != NULL)
         tb_printf(tb, " Reason=%s State=%s", vhpi_cb_reason_str(cb->Reason),
                   vhpi_state_str(cb->State));

      c_range *r = is_range(obj);
      if (r != NULL)
         tb_printf(tb, " IsNull=%d IsUp=%d IsUnconstrained=%d",
                   r->IsNull, r->IsUp, r->IsUnconstrained);

      c_intRange *ir = is_intRange(obj);
      if (ir != NULL)
         tb_printf(tb, " LeftBound=%"PRIi64" RightBound=%"PRIi64,
                   ir->LeftBound, ir->RightBound);
   }

   tb_append(tb, '}');
}

static const char *handle_pp(vhpiHandleT handle)
{
   static __thread text_buf_t *tb = NULL;
   if (tb == NULL)
      tb = tb_new();

   tb_rewind(tb);
   handle_pp_r(handle, tb);
   return tb_get(tb);
}

static const char *cb_data_pp(const vhpiCbDataT *data)
{
   static __thread text_buf_t *tb = NULL;
   if (tb == NULL)
      tb = tb_new();

   tb_rewind(tb);
   tb_printf(tb, "{reason=%s obj=", vhpi_cb_reason_str(data->reason));
   handle_pp_r(data->obj, tb);
   tb_printf(tb, " cb_rtn=%p user_data=%p}", data->cb_rtn, data->user_data);

   return tb_get(tb);
}

static void *new_object(size_t size, vhpiClassKindT class)
{
   assert(size >= sizeof(c_vhpiObject));

   c_vhpiObject *obj = pool_calloc(vhpi_context()->pool, size);
   obj->kind = class;

   return obj;
}

static void *recycle_object(size_t size, vhpiClassKindT class)
{
   vhpi_context_t *c = vhpi_context();

   for (int i = 0; i < c->recycle.count; i++) {
      c_vhpiObject *obj = c->recycle.items[i];
      if (obj->kind == class) {
         for (int j = i; j < c->recycle.count - 1; j++)
            c->recycle.items[j] = c->recycle.items[j + 1];
         ATRIM(c->recycle, c->recycle.count - 1);

         memset(obj, '\0', size);
         obj->kind = class;

         return obj;
      }
   }

   return new_object(size, class);
}

static vhpiCharT *new_string(const char *s)
{
   vhpi_context_t *c = vhpi_context();
   if (c->strtab == NULL)
      c->strtab = shash_new(1024);

   vhpiCharT *p = shash_get(c->strtab, s);
   if (p == NULL) {
      const size_t len = strlen(s);
      vhpiCharT *copy = p = pool_malloc(c->pool, len + 1);
      memcpy(copy, s, len + 1);
      shash_put(c->strtab, s, copy);
   }

   return p;
}

__attribute__((format(printf, 1, 2)))
static vhpiCharT *new_stringf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *LOCAL buf = xvasprintf(fmt, ap);
   va_end(ap);

   return new_string(buf);
}

static void vhpi_list_reserve(vhpiObjectListT *list, unsigned num)
{
   if (list->limit >= num)
      return;

   assert(list->count == 0);
   assert(list->items == NULL);

   mem_pool_t *mp = vhpi_context()->pool;

   list->limit = num;
   list->items = pool_malloc_array(mp, num, sizeof(c_vhpiObject *));
}

static inline void vhpi_list_add(vhpiObjectListT *list, c_vhpiObject *obj)
{
   assert(list->count < list->limit);
   list->items[list->count++] = obj;
}

static void init_abstractRegion(c_abstractRegion *r, c_abstractRegion *upper,
                                tree_t t)
{
   r->object.tree = t;
   r->UpperRegion = upper;
   r->Decls.fn = vhpi_lazy_decls;
   r->Stmts.fn = vhpi_lazy_stmts;
}

static void init_designInstUnit(c_designInstUnit *iu, c_abstractRegion *upper,
                                tree_t t, c_designUnit *u)
{
   init_abstractRegion(&(iu->region), upper, t);

   iu->DesignUnit = u;
}

static void init_abstractDecl(c_abstractDecl *d, tree_t t, c_abstractRegion *r)
{
   d->object.tree = t;
   d->ImmRegion = r;
}

static void init_objDecl(c_objDecl *d, tree_t t, c_abstractRegion *ImmRegion)
{
   init_abstractDecl(&(d->decl), t, ImmRegion);

   d->offset = PTRDIFF_MAX;

   tree_flags_t flags = tree_flags(t);
   if (flags & TREE_F_LOCALLY_STATIC)
      d->Staticness = vhpiLocallyStatic;
   else if (flags & TREE_F_GLOBALLY_STATIC)
      d->Staticness = vhpiGloballyStatic;
   else
      d->Staticness = vhpiDynamic;

   d->SelectedNames.fn = vhpi_lazy_selected_names;
   d->IndexedNames.fn = vhpi_lazy_indexed_names;
}

static void init_interfaceDecl(c_interfaceDecl *d, tree_t t,
                               int Position, c_abstractRegion *ImmRegion)
{
   init_objDecl(&(d->objDecl), t, ImmRegion);
   d->Position = Position;
}

static void init_paramDecl(c_paramDecl *d, tree_t t, int Position,
                           int argslot, c_abstractRegion *ImmRegion)
{
   init_interfaceDecl(&(d->interface), t, Position, ImmRegion);
   d->argslot = argslot;
}

static void init_subpDecl(c_subpDecl *d, tree_t t, c_abstractRegion *ImmRegion)
{
   init_abstractDecl(&(d->decl), t, ImmRegion);
}

static void init_elemDecl(c_elemDecl *ed, tree_t t, c_abstractDecl *parent)
{
   init_abstractDecl(&(ed->decl), t, parent->ImmRegion);
   ed->parent = parent;
   ed->Position = tree_pos(t);
}

static void init_typeDecl(c_typeDecl *d, tree_t t, type_t type,
                          c_abstractRegion *ImmRegion)
{
   init_abstractDecl(&(d->decl), t, ImmRegion);

   d->offset = PTRDIFF_MAX;
   d->type = type;
   d->format = vhpi_format_for_type(d->type, &d->map_str);
   d->homogeneous = type_is_homogeneous(type);
   d->IsUnconstrained = type_is_unconstrained(type);
   d->IsScalar = type_is_scalar(type);
   d->IsComposite = type_is_composite(type);

   // TODO: use layout for this instead
   if (type_is_array(type) && d->homogeneous)
      d->elem_bytes = type_byte_width(type_elem_recur(type));
}

static void init_scalarTypeDecl(c_scalarTypeDecl *d, tree_t t, type_t type,
                                c_abstractRegion *ImmRegion)
{
   init_typeDecl(&(d->typeDecl), t, type, ImmRegion);
   d->typeDecl.elem_bytes = (type_bit_width(type) + 7) / 8;
}

static void init_compositeTypeDecl(c_compositeTypeDecl *d, tree_t t,
                                   type_t type, c_abstractRegion *ImmRegion)
{
   init_typeDecl(&(d->typeDecl), t, type, ImmRegion);
}

static void init_enumLiteral(c_enumLiteral *el, tree_t t, c_enumTypeDecl *Type)
{
   init_abstractDecl(&(el->decl), t, Type->scalar.typeDecl.decl.ImmRegion);
   el->Position = tree_pos(t);
}

static void init_unitDecl(c_unitDecl *ud, tree_t t, c_physTypeDecl *Type)
{
   init_abstractDecl(&(ud->decl), t, NULL);
   ud->Type = Type;

   const int64_t value = assume_int(tree_value(t));
   ud->PhysPosition.high = value >> 32;
   ud->PhysPosition.low = value & 0xffffffff;
}

static void init_range(c_range *r, vhpiBooleanT IsUp, vhpiBooleanT IsNull,
                       vhpiBooleanT IsDiscrete)
{
   r->IsUp = IsUp;
   r->IsNull = IsNull;
   r->IsDiscrete = IsDiscrete;
   r->LeftExpr = NULL;
   r->RightExpr = NULL;
}

static void init_expr(c_expr *e, vhpiStaticnessT Staticness, c_typeDecl *Type)
{
   e->Type = Type;
   e->Staticness = Staticness;
}

static void init_stmt(c_stmt *s, tree_t t)
{
   s->IsSeqStmt = false;
   s->LabelName = new_string(istr(tree_ident(t)));
}

static void init_name(c_name *n, vhpiStaticnessT Staticness, c_typeDecl *Type,
                      vhpiStringT Name, vhpiStringT FullName)
{
   init_expr(&(n->expr), Staticness, Type);

   n->Name = Name;
   n->FullName = FullName;
   n->SelectedNames.fn = vhpi_lazy_selected_names;
   n->IndexedNames.fn = vhpi_lazy_indexed_names;
}

static void init_prefixedName(c_prefixedName *pn, c_typeDecl *Type,
                              c_vhpiObject *prefix, const char *suffix)
{
   vhpiStringT Name = NULL, FullName = NULL;
   vhpiStaticnessT Staticness = vhpiDynamic;

   c_name *name = is_name(prefix);
   if (name != NULL) {
      Name = new_stringf("%s%s", vhpi_get_name(prefix), suffix);
      FullName = new_stringf("%s%s", vhpi_get_full_name(prefix), suffix);
   }

   c_objDecl *obj = is_objDecl(prefix);
   if (obj != NULL) {
      Name = new_stringf("%s%s", vhpi_get_name(prefix), suffix);
      FullName = new_stringf("%s%s", vhpi_get_full_name(prefix), suffix);
      Staticness = obj->Staticness;
   }

   init_name(&(pn->name), Staticness, Type, Name, FullName);
   pn->Prefix = prefix;
}

static void init_indexedName(c_indexedName *in, c_typeDecl *Type,
                             c_vhpiObject *prefix, vhpiObjectListT *Constraints,
                             vhpiIntT indices[])
{
   vhpiIntT BaseIndex = 0;

   LOCAL_TEXT_BUF suffix = tb_new();
   tb_append(suffix, '(');
   for (int i = 0; i < Constraints->count; i++) {
      if (i != 0)
         tb_append(suffix, ',');

      c_intRange *ir = is_intRange(Constraints->items[i]);
      assert(ir != NULL);

      int idx;
      if (ir->range.IsUp) {
         idx = indices[i] - ir->LeftBound;
         BaseIndex *= ir->RightBound - ir->LeftBound + 1;
      }
      else {
         idx = ir->LeftBound - indices[i];
         BaseIndex *= ir->LeftBound - ir->RightBound + 1;
      }

      BaseIndex += indices[i];
      tb_printf(suffix, "%d", idx);
   }
   tb_append(suffix, ')');

   init_prefixedName(&(in->prefixedName), Type, prefix, tb_get(suffix));
   in->BaseIndex = BaseIndex;

   vhpiIntT num_elems = 1;
   if (Type->homogeneous) {
      num_elems = vhpi_get_size(&(Type->decl.object));
      assert(num_elems != vhpiUndefined);
   }

   in->offset = BaseIndex * num_elems;
}

static bool range_sync(c_intRange *ir)
{
   if (!ir->range.IsUnconstrained)
      return true;
   else if (ir->source == NULL)
      return false;

   vhpi_ptr_t ptr = vhpi_get_ptr(ir->source);
   assert(ptr.kind != PTR_NULL);
   assert(ptr.bounds != NULL);

   const ffi_dim_t *dim = &(ptr.bounds[ir->nth_dim]);

   ir->LeftBound = dim->left;
   ir->RightBound = ffi_array_right(dim->left, dim->length);
   ir->range.IsUp = ffi_array_dir(dim->length) == RANGE_TO;
   ir->range.IsNull = ffi_array_length(dim->length) == 0;

   return true;
}

static vhpiIntT range_len(c_intRange *ir)
{
   if (!range_sync(ir))
      return vhpiUndefined;
   else if (ir->range.IsNull)
      return 0;
   else if (ir->range.IsUp)
      return MAX(ir->RightBound - ir->LeftBound + 1, 0);
   else
      return MAX(ir->LeftBound - ir->RightBound + 1, 0);
}

static void init_selectedName(c_selectedName *sn, c_vhpiObject *prefix,
                              c_elemDecl *Suffix)
{
   assert(prefix != NULL);

   LOCAL_TEXT_BUF suffix = tb_new();
   tb_append(suffix, '.');
   tb_cat(suffix, (const char *)vhpi_get_name(&Suffix->decl.object));

   c_typeDecl *td = vhpi_get_type(&(Suffix->decl.object));
   assert(td != NULL);

   init_prefixedName(&(sn->prefixedName), td, prefix, tb_get(suffix));
   sn->Suffix = Suffix;
}

static void init_designUnit(c_designUnit *u, tree_t t)
{
   init_abstractRegion(&(u->region), NULL, t);

   u->UnitName = new_string(istr(tree_ident(t)));
}

static void init_secondaryUnit(c_secondaryUnit *u, tree_t t, c_designUnit *p)
{
   init_designUnit(&(u->designUnit), t);
   u->PrimaryUnit = p;
}

static void init_entityDecl(c_entityDecl *e, tree_t t)
{
   init_designUnit(&(e->designUnit), t);
}

static void init_packDecl(c_packDecl *p, tree_t t)
{
   init_designUnit(&(p->designUnit), t);
}

static void init_verilogModule(c_verilogModule *mod, tree_t t)
{
   init_designUnit(&(mod->designUnit), t);
}

static vhpiObjectListT *expand_lazy_list(c_vhpiObject *obj, vhpiLazyListT *lazy)
{
   vhpiLazyFnT fn = lazy->fn;
   if (fn != NULL) {
      lazy->fn = NULL;   // Avoid infinite recursion
      (*fn)(obj);
   }

   return &(lazy->list);
}

static void init_refcounted(c_refcounted *rc)
{
   rc->refcount = 0;  // No handles point to this object
}

static bool init_iterator(c_iterator *it, vhpiOneToManyT type,
                          c_vhpiObject *obj)
{
   init_refcounted(&it->refcounted);

   if (obj == NULL) {
      switch (type) {
      case vhpiPackInsts:
         it->list = &(vhpi_context()->packages);
         return true;
      default:
         return false;
      }
   }

   c_abstractRegion *region = is_abstractRegion(obj);
   if (region != NULL) {
      switch (type) {
      case vhpiDecls:
         it->list = expand_lazy_list(obj, &(region->Decls));
         return true;
      case vhpiInternalRegions:
         it->filter = (vhpiFilterT)is_abstractRegion;
         it->list = expand_lazy_list(obj, &(region->Stmts));
         return true;
      case vhpiConstDecls:
         it->filter = (vhpiFilterT)is_constDecl;
         it->list = expand_lazy_list(obj, &(region->Decls));
         return true;
      case vhpiVarDecls:
         it->filter = (vhpiFilterT)is_varDecl;
         it->list = expand_lazy_list(obj, &(region->Decls));
         return true;
      case vhpiSigDecls:
         it->filter = (vhpiFilterT)is_sigDecl;
         it->list = expand_lazy_list(obj, &(region->Decls));
         return true;
      case vhpiGenericDecls:
         it->filter = (vhpiFilterT)is_genericDecl;
         it->list = expand_lazy_list(obj, &(region->Decls));
         return true;
      case vhpiPortDecls:
         it->filter = (vhpiFilterT)is_portDecl;
         it->list = expand_lazy_list(obj, &(region->Decls));
         return true;
      case vhpiStmts:
         it->list = expand_lazy_list(obj, &(region->Stmts));
         return true;
      case vhpiBlockStmts:
         it->filter = (vhpiFilterT)is_blockStmt;
         it->list = expand_lazy_list(obj, &(region->Stmts));
         return true;
      case vhpiCompInstStmts:
         it->filter = (vhpiFilterT)is_compInstStmt;
         it->list = expand_lazy_list(obj, &(region->Stmts));
         return true;
      default:
         return false;
      }
   }

   c_subpDecl *subp = is_subpDecl(obj);
   if (subp != NULL) {
      switch (type) {
      case vhpiParamDecls:
         it->list = &(subp->Params);
         return true;
      default:
         return false;
      }
   }

   c_enumTypeDecl *etd = is_enumTypeDecl(obj);
   if (etd != NULL) {
      if (type == vhpiEnumLiterals) {
         it->list = expand_lazy_list(obj, &(etd->EnumLiterals));
         return true;
      }
      return false;
   }

   c_recordTypeDecl *record = is_recordTypeDecl(obj);
   if (record != NULL) {
      if (type == vhpiRecordElems) {
         it->list = expand_lazy_list(obj, &(record->RecordElems));
         return true;
      }
      return false;
   }

   c_subTypeDecl *subtype = is_subTypeDecl(obj);
   if (subtype != NULL) {
      if (type == vhpiConstraints) {
         it->list = expand_lazy_list(obj, &(subtype->Constraints));
         return true;
      }
      return false;
   }

   c_physTypeDecl *ptd = is_physTypeDecl(obj);
   if (ptd != NULL) {
      switch (type) {
      case vhpiConstraints:
         it->single = &(ptd->constraint->object);
         return true;
      case vhpiUnitDecls:
         it->list = expand_lazy_list(obj, &(ptd->UnitDecls));
         return true;
      default:
         return false;
      }
   }

   c_intTypeDecl *itd = is_intTypeDecl(obj);
   if (itd != NULL) {
      if (type == vhpiConstraints) {
         it->single = &(itd->constraint->object);
         return true;
      }
      return false;
   }

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL) {
      switch(type) {
      case vhpiIndexedNames:
         it->list = expand_lazy_list(obj, &(od->IndexedNames));
         return true;
      case vhpiSelectedNames:
         it->list = expand_lazy_list(obj, &(od->SelectedNames));
         return true;
      default:
         return false;
      }
   }

   c_name *n = is_name(obj);
   if (n != NULL) {
      switch (type) {
      case vhpiIndexedNames:
         it->list = expand_lazy_list(obj, &(n->IndexedNames));
         return true;
      case vhpiSelectedNames:
         it->list = expand_lazy_list(obj, &(n->SelectedNames));
         return true;
      default:
         return false;
      }
   }

   c_tool *t = is_tool(obj);
   if (t != NULL) {
      if (type == vhpiArgvs) {
         it->list = &(t->argv);
         return true;
      }
      return false;
   }

   return false;
}

static void init_callback(c_callback *cb, const vhpiCbDataT *data, int flags)
{
   init_refcounted(&cb->refcounted);

   cb->Reason = data->reason;
   cb->State  = (flags & vhpiDisableCb) ? vhpiDisable : vhpiEnable;
   cb->data   = *data;
}

static void vhpi_signal_event_cb(uint64_t now, rt_signal_t *signal,
                                 rt_watch_t *watch, void *user)
{
   c_vhpiObject *obj = from_handle(user);
   if (obj == NULL)
      return;

   c_callback *cb = is_callback(obj);
   if (cb == NULL)
      return;

   if (cb->State != vhpiEnable)
      return;

   vhpiTimeT time;
   if (cb->data.time != NULL) {
      vhpi_get_time(&time, NULL);
      cb->data.time = &time;
   }

   (cb->data.cb_rtn)(&(cb->data));
}

static void vhpi_global_cb(rt_model_t *m, void *user)
{
   vhpiHandleT handle = user;
   vhpi_context_t *c = vhpi_context();

  {
     handle_slot_t *slot = decode_handle(c, handle);
     if (slot == NULL)
        return;

     c_callback *cb = is_callback(slot->obj);
     if (cb == NULL)
        return;

     assert(cb->State != vhpiMature);

     if (cb->State == vhpiEnable)
        (cb->data.cb_rtn)(&(cb->data));
   }

   // The handle may be invalidated by the user call

   {
      handle_slot_t *slot = decode_handle(c, handle);
      if (slot == NULL)
         return;

      c_callback *cb = is_callback(slot->obj);
      assert(cb != NULL);

      switch (cb->Reason) {
      case vhpiCbRepEndOfProcesses:
      case vhpiCbRepLastKnownDeltaCycle:
      case vhpiCbRepNextTimeStep:
      case vhpiCbRepEndOfTimeStep:
      case vhpiCbRepStartOfNextCycle:
      case vhpiCbValueChange:
         break;    // Repetitive

      default:
         cb->State = vhpiMature;
         drop_handle(c, handle);
         break;
      }
   }
}

static void vhpi_collect_signals(c_vhpiObject *obj, slice_list_t *list)
{
   vhpi_ptr_t ptr = vhpi_get_ptr(obj);
   assert(ptr.kind != PTR_NULL);
   if (ptr.signal != NULL) {
      vhpiIntT count = vhpi_get_size(obj);
      if (count == vhpiUndefined)
         vhpi_error(vhpiInternal, obj_loc(obj), "cannot get size of object");
      else {
         vhpi_slice_t slice = {
            .signal = ptr.signal,
            .offset = ptr.offset,
            .count  = count,
         };
         APUSH(*list, slice);
      }
   }
   else {
      // Must be composite non-homogenous object
      c_iterator it = {};

      if (init_iterator(&it, vhpiSelectedNames, obj)) {
         for (int i = 0; i < it.list->count; i++)
            vhpi_collect_signals(it.list->items[i], list);
      }

      if (init_iterator(&it, vhpiIndexedNames, obj)) {
         for (int i = 0; i < it.list->count; i++)
            vhpi_collect_signals(it.list->items[i], list);
      }
   }
}

static c_typeDecl *find_typeDecl(type_t type, c_abstractRegion *r)
{
   assert(!is_anonymous_subtype(type));

   c_abstractRegion *upper = NULL;
   vhpiObjectListT *decls;
   tree_t container = type_container(type);
   switch (tree_kind(container)) {
   case T_PACKAGE:
      {
         c_designUnit *du = cached_designUnit(container);

         // TODO: this should be safe for hash lookup
         decls = expand_lazy_list(&du->region.object, &(du->region.Decls));
      }
      break;
   case T_ARCH:
   case T_ELAB:
      {
         assert(r != NULL);   // TODO: find in lexical scope

         upper = r->UpperRegion;
         decls = expand_lazy_list(&r->object, &(r->Decls));
      }
      break;
   default:
      fatal_trace("cannot handle container kind %pK for %pT in find_typeDecl",
                  container, type);
   }

   const type_kind_t kind = type_kind(type);

   for (int i = 0; i < decls->count; i++) {
      c_typeDecl *td = is_typeDecl(decls->items[i]);
      if (td == NULL)
         continue;
      else if (td->type == type)
         return td;
      else if (kind == T_ARRAY) {
         c_subTypeDecl *std = is_subTypeDecl(decls->items[i]);
         if (std == NULL || std->BaseType == NULL)
            continue;
         else if (std->BaseType->type == type)
            return std->BaseType;
      }
      else if (kind == T_INCOMPLETE && type_ident(type) == type_ident(td->type))
         return td;
   }

   if (upper != NULL)
      return find_typeDecl(type, upper);

   fatal_trace("missing type %pK %pT in %pK %pI", type, type, container,
               tree_ident(container));
}

static c_typeDecl *new_anonymous_subtypeDecl(type_t type, c_vhpiObject *source,
                                             c_abstractRegion *ImmRegion,
                                             unsigned depth)
{
   c_subTypeDecl *std = new_object(sizeof(c_subTypeDecl), vhpiSubtypeDeclK);
   init_typeDecl(&(std->typeDecl), source->tree, type, ImmRegion);

   std->source = source;
   std->depth = depth;
   std->Size = vhpiUndefined;
   std->IsAnonymous = true;
   std->Constraints.fn = vhpi_lazy_constraints;

   return &(std->typeDecl);
}

static c_typeDecl *vhpi_get_type(c_vhpiObject *obj)
{
   c_typeDecl *td = is_typeDecl(obj);
   if (td != NULL)
      return td;

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL && od->Type != NULL)
      return od->Type;
   else if (od != NULL) {
      c_abstractRegion *r = od->decl.ImmRegion;
      type_t type = tree_type(od->decl.object.tree);
      if (is_anonymous_subtype(type))
         return (od->Type = new_anonymous_subtypeDecl(type, obj, r, 0));
      else {
         c_typeDecl *td = find_typeDecl(type, r);
         if (td->IsUnconstrained)
            return (od->Type = new_anonymous_subtypeDecl(type, obj, r, 0));
         else
            return (od->Type = td);
      }
   }

   c_expr *e = is_expr(obj);
   if (e != NULL)
      return e->Type;

   c_elemDecl *ed = is_elemDecl(obj);
   if (ed != NULL && ed->Type != NULL)
      return ed->Type;
   else if (ed != NULL) {
      c_abstractRegion *r = ed->decl.ImmRegion;
      type_t type = tree_type(ed->decl.object.tree);
      if (is_anonymous_subtype(type))
         return (ed->Type = new_anonymous_subtypeDecl(type, obj, r, 0));
      else
         return (ed->Type = find_typeDecl(type, r));
   }

   c_funcDecl *fd = is_funcDecl(obj);
   if (fd != NULL && fd->ReturnType != NULL)
      return fd->ReturnType;
   else if (fd != NULL) {
      c_abstractRegion *r = fd->subpDecl.decl.ImmRegion;
      type_t result = type_result(tree_type(fd->subpDecl.decl.object.tree));
      return (fd->ReturnType = find_typeDecl(result, r));
   }

   c_unitDecl *ud = is_unitDecl(obj);
   if (ud != NULL)
      return &(ud->Type->scalar.typeDecl);

   return NULL;
}

static c_typeDecl *vhpi_get_base_type(c_vhpiObject *obj)
{
   c_typeDecl *td = vhpi_get_type(obj);
   if (td == NULL)
      return NULL;

   c_subTypeDecl *std = is_subTypeDecl(&(td->decl.object));
   if (std != NULL && std->BaseType != NULL)
      return std->BaseType;
   else if (std != NULL) {
      type_t base = type_base_recur(std->typeDecl.type);
      c_abstractRegion *r = std->typeDecl.decl.ImmRegion;
      return (std->BaseType = find_typeDecl(base, r));
   }

   return td;
}

static c_typeDecl *vhpi_get_elem_type(c_vhpiObject *obj)
{
   c_typeDecl *td = vhpi_get_type(obj);
   if (td == NULL)
      return NULL;

   c_arrayTypeDecl *atd = is_arrayTypeDecl(&(td->decl.object));
   if (atd != NULL && atd->ElemType != NULL)
      return atd->ElemType;
   else if (atd != NULL) {
      type_t type = type_elem(atd->composite.typeDecl.type);
      c_abstractDecl *d = &(atd->composite.typeDecl.decl);
      c_abstractRegion *r = d->ImmRegion;
      if (is_anonymous_subtype(type))
         return (atd->ElemType = new_anonymous_subtypeDecl(type, obj, r, 1));
      else
         return (atd->ElemType = find_typeDecl(type, r));
   }

   c_subTypeDecl *std = is_subTypeDecl(&(td->decl.object));
   if (std != NULL && std->typeDecl.IsScalar)
      return NULL;
   else if (std != NULL && std->ElemType != NULL)
      return std->ElemType;
   else if (std != NULL) {
      // TODO: what if subtype imposes no additional constraints on elem?
      type_t type = type_elem(std->typeDecl.type);
      c_abstractRegion *r = std->typeDecl.decl.ImmRegion;
      if (is_anonymous_subtype(type)) {
         c_vhpiObject *source = std->source ?: &(std->typeDecl.decl.object);
         return (std->ElemType = new_anonymous_subtypeDecl(type, source, r,
                                                           std->depth + 1));
      }
      else
         return (std->ElemType = find_typeDecl(type, r));
   }

   return NULL;
}

static c_typeDecl *vhpi_get_val_type(c_vhpiObject *obj)
{
   c_accessTypeDecl *atd = is_accessTypeDecl(obj);
   if (atd != NULL && atd->ValType != NULL)
      return atd->ValType;
   else if (atd != NULL) {
      type_t designated = type_designated(atd->typeDecl.type);
      c_abstractRegion *r = atd->typeDecl.decl.ImmRegion;
      return (atd->ValType = find_typeDecl(designated, r));
   }

   c_fileTypeDecl *ftd = is_fileTypeDecl(obj);
   if (ftd != NULL && ftd->ValType != NULL)
      return ftd->ValType;
   else if (ftd != NULL) {
      type_t designated = type_designated(ftd->typeDecl.type);
      c_abstractRegion *r = ftd->typeDecl.decl.ImmRegion;
      return (ftd->ValType = find_typeDecl(designated, r));
   }

   return NULL;
}

static vhpiIntT vhpi_get_size(c_vhpiObject *obj)
{
   c_subTypeDecl *std = is_subTypeDecl(obj);
   if (std != NULL && std->typeDecl.IsScalar)
      return 1;
   else if (std != NULL && std->Size != vhpiUndefined)
      return std->Size;
   else if (std != NULL) {
      vhpiObjectListT *cons =
         expand_lazy_list(&std->typeDecl.decl.object, &std->Constraints);

      vhpiIntT size = 1;
      for (int i = 0; i < cons->count; i++) {
         vhpiIntT dim = vhpi_get_size(cons->items[i]);
         if (dim == vhpiUndefined)
            return vhpiUndefined;

         size *= dim;
      }

      c_typeDecl *elem = vhpi_get_elem_type(obj);
      if (elem != NULL) {
         vhpiIntT elem_size = vhpi_get_size(&(elem->decl.object));
         if (elem_size == vhpiUndefined)
            return vhpiUndefined;

         size *= elem_size;
      }

      return (std->Size = size);
   }

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL) {
      c_typeDecl *td = vhpi_get_type(obj);
      vhpiIntT num_elems = vhpi_get_size(&(td->decl.object));
      if (num_elems != vhpiUndefined)
         return num_elems;

      vhpi_ptr_t ptr = vhpi_get_ptr(obj);
      if (ptr.kind == PTR_NULL || ptr.bounds == NULL)
         return vhpiUndefined;

      // TODO: handle nested / multiple dimensions
      return ffi_array_length(ptr.bounds[0].length);
   }

   c_intRange *ir = is_intRange(obj);
   if (ir != NULL)
      return range_len(ir);

   c_typeDecl *td = is_typeDecl(obj);
   if (td != NULL && td->IsScalar)
      return 1;

   c_expr *e = is_expr(obj);
   if (e != NULL) {
      c_typeDecl *td = vhpi_get_type(obj);
      vhpiIntT num_elems = vhpi_get_size(&(td->decl.object));
      if (num_elems != vhpiUndefined)
         return num_elems;

      vhpi_ptr_t ptr = vhpi_get_ptr(obj);
      if (ptr.kind == PTR_NULL || ptr.bounds == NULL)
         return vhpiUndefined;

      // TODO: handle nested / multiple dimensions
      return ffi_array_length(ptr.bounds[0].length);
   }

   c_funcDecl *fd = is_funcDecl(obj);
   if (fd != NULL && fd->size != vhpiUndefined)
      return fd->size;
   else if (fd != NULL) {
      c_typeDecl *td = vhpi_get_type(obj);
      return vhpi_get_size(&(td->decl.object));
   }

   VHPI_DEBUG("undefined for %s", vhpi_class_str(obj->kind));
   return vhpiUndefined;
}

static bool vhpi_is_signal(c_vhpiObject *obj)
{
   if (is_sigDecl(obj) || is_portDecl(obj))
       return true;

   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL)
      return vhpi_is_signal(pn->Prefix);

   return false;
}

static const jit_layout_t *vhpi_get_layout(c_vhpiObject *obj)
{
   // TODO: cache this somewhere?

   c_typeDecl *td = vhpi_get_type(obj);
   assert(td != NULL);

   if (vhpi_is_signal(obj))
      return signal_layout_of(td->type);
   else
      return layout_of(td->type);
}

static vhpi_ptr_t vhpi_ptr_from_layout(const jit_layout_t *l, void *mem)
{
   vhpi_ptr_t ptr = { .kind = PTR_RAW, .data = mem };

   for (int i = 0; i < l->nparts; i++) {
      switch (l->parts[i].class) {
      case LC_EXTERNAL:
         ptr.data = *(void **)(mem + l->parts[i].offset);
         break;
      case LC_SIGNAL:
         {
            sig_shared_t *ss = *(sig_shared_t **)(mem + l->parts[i].offset);
            ptr.kind = PTR_SIGNAL;
            ptr.data = ss->data;
            ptr.signal = container_of(ss, rt_signal_t, shared);
         }
         break;
      case LC_OFFSET:
         ptr.offset = *(uint32_t *)(mem + l->parts[i].offset);
         break;
      case LC_BOUNDS:
         ptr.bounds = mem + l->parts[i].offset;
         break;
      case LC_DATA:
         break;
      }
   }

   return ptr;
}

static vhpi_ptr_t vhpi_get_ptr(c_vhpiObject *obj)
{
   c_paramDecl *pd = is_paramDecl(obj);
   if (pd != NULL) {
      vhpi_context_t *c = vhpi_context();
      assert(c->args != NULL);

      vhpi_ptr_t ptr = { .kind = PTR_RAW };

      if (pd->wrapped) {
         ptr.data = c->args[pd->argslot].pointer;
         ptr.bounds = (void *)&(c->args[pd->argslot + 1]);
      }
      else {
         c_typeDecl *td = vhpi_get_type(obj);
         assert(td != NULL);

         if (td->IsComposite)
            ptr.data = c->args[pd->argslot].pointer;
         else {
            c_varParamDecl *vpd = is_varParamDecl(obj);
            if (vpd != NULL && vpd->Mode != vhpiInMode)
               ptr.data = c->args[pd->argslot].pointer;
            else
               ptr.data = (void *)&(c->args[pd->argslot]);
         }
      }

      return ptr;
   }

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL) {
      vhpi_ptr_t ctx = vhpi_get_ptr(&(od->decl.ImmRegion->object));
      if (ctx.kind == PTR_NULL)
         return (vhpi_ptr_t){};

      void *var;
      if (od->offset == PTRDIFF_MAX) {
         vhpi_context_t *c = vhpi_context();
         assert(c->phase == vhpiSimulationPhase);

         var = jit_get_frame_var(c->jit, od->decl.ImmRegion->handle, ctx.data,
                                 tree_ident(od->decl.object.tree));
         if (var == NULL)
            fatal_trace("cannot get pointer to %s", vhpi_get_full_name(obj));

         od->offset = var - ctx.data;
      }
      else
         var = ctx.data + od->offset;

      const jit_layout_t *l = vhpi_get_layout(obj);
      return vhpi_ptr_from_layout(l, var);
   }

   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL) {
      vhpi_ptr_t base = vhpi_get_ptr(pn->Prefix);
      if (base.kind == PTR_NULL)
         return (vhpi_ptr_t){};

      const jit_layout_t *l = vhpi_get_layout(pn->Prefix);

      c_selectedName *sn = is_selectedName(obj);
      if (sn != NULL) {
         assert(base.kind == PTR_RAW);
         assert(sn->Suffix->Position < l->nparts);

         void *fptr = base.data + l->parts[sn->Suffix->Position].offset;

         const jit_layout_t *fl = vhpi_get_layout(obj);
         return vhpi_ptr_from_layout(fl, fptr);
      }

      c_indexedName *in = is_indexedName(obj);
      if (in != NULL) {
         switch (base.kind) {
         case PTR_SIGNAL:
            return (vhpi_ptr_t){
               .kind = PTR_SIGNAL,
               .data = base.data,
               .offset = base.offset + in->offset,
               .signal = base.signal,
            };
         case PTR_RAW:
            {
               const jit_layout_t *l = vhpi_get_layout(obj);
               void *data = base.data + in->BaseIndex * l->size;
               return vhpi_ptr_from_layout(l, data);
            }
         default:
            should_not_reach_here();
         }
      }

      vhpi_error(vhpiInternal, obj_loc(obj), "unsupported prefixed name %s",
                 vhpi_class_str(obj->kind));
      return (vhpi_ptr_t){};
   }

   c_abstractRegion *r = is_abstractRegion(obj);
   if (r != NULL && r->privdata != NULL)
      return (vhpi_ptr_t){ .kind = PTR_RAW, .data = r->privdata };
   else if (r != NULL) {
      vhpi_context_t *c = vhpi_context();
      if (c->phase != vhpiSimulationPhase)
         return (vhpi_ptr_t){};

      if (is_concurrent_block(r->object.tree)) {
         tree_t h = tree_decl(r->object.tree, 0);
         assert(tree_kind(h) == T_HIER);

         r->handle = jit_compile(c->jit, tree_ident2(h));

         rt_scope_t *s = find_scope(c->model, r->object.tree);
         if (s == NULL)
            fatal_trace("missing scope for %s", vhpi_get_full_name(obj));

         r->privdata = *mptr_get(s->privdata);
      }
      else {
         r->handle = jit_compile(c->jit, tree_ident(r->object.tree));
         r->privdata = jit_link(c->jit, r->handle);
      }

      return (vhpi_ptr_t){ .kind = PTR_RAW, .data = r->privdata };
   }

   c_subTypeDecl *std = is_subTypeDecl(obj);
   if (std != NULL && std->source != NULL)
      return vhpi_get_ptr(std->source);
   else if (std != NULL)
      assert(!is_anonymous_subtype(std->typeDecl.type));

   c_typeDecl *td = is_typeDecl(obj);
   if (td != NULL) {
      vhpi_ptr_t ctx = vhpi_get_ptr(&(td->decl.ImmRegion->object));
      if (ctx.kind == PTR_NULL)
         return (vhpi_ptr_t){};

      void *var;
      if (td->offset == PTRDIFF_MAX) {
         vhpi_context_t *c = vhpi_context();
         assert(c->phase == vhpiSimulationPhase);

         var = jit_get_frame_var(c->jit, td->decl.ImmRegion->handle,
                                 ctx.data, type_ident(td->type));
         if (var == NULL)
            fatal_trace("cannot get pointer to type %pT", td->type);

         td->offset = var - ctx.data;
      }
      else
         var = ctx.data + td->offset;

      const jit_layout_t *l = vhpi_get_layout(obj);
      return vhpi_ptr_from_layout(l, var);
   }

   c_elemDecl *ed = is_elemDecl(obj);
   if (ed != NULL) {
      vhpi_ptr_t base = vhpi_get_ptr(&(ed->parent->object));
      assert(base.kind != PTR_NULL);

      const jit_layout_t *l = vhpi_get_layout(&(ed->parent->object));
      assert(ed->Position < l->nparts);

      void *fptr = base.data + l->parts[ed->Position].offset;

      const jit_layout_t *fl = vhpi_get_layout(obj);
      return vhpi_ptr_from_layout(fl, fptr);
   }

   c_funcDecl *fd = is_funcDecl(obj);
   if (fd != NULL) {
      vhpi_context_t *c = vhpi_context();
      assert(c->args != NULL);

      c_typeDecl *td = vhpi_get_type(obj);
      assert(td != NULL);

      vhpi_ptr_t ptr = { .kind = PTR_RAW };

      if (td->IsScalar)
         ptr.data = (void *)&(c->args[0]);
      else
         ptr.data = c->args[0].pointer;

      return ptr;
   }

   VHPI_DEBUG("cannot get pointer to %s", vhpi_class_str(obj->kind));
   return (vhpi_ptr_t){};
}

static vhpiStringT vhpi_get_name(c_vhpiObject *obj)
{
   c_subTypeDecl *std = is_subTypeDecl(obj);
   if (std != NULL && std->typeDecl.decl.Name != NULL)
      return std->typeDecl.decl.Name;
   else if (std != NULL && std->IsAnonymous) {
      c_typeDecl *base = vhpi_get_base_type(obj);
      assert(base != NULL);
      return (std->typeDecl.decl.Name = vhpi_get_name(&(base->decl.object)));
   }

   c_abstractRegion *r = is_abstractRegion(obj);
   if (r != NULL && r->Name != NULL)
      return r->Name;
   else if (r != NULL) {
      const char *qual = istr(tree_ident(r->object.tree));
      if (is_package(r->object.tree)) {
         LOCAL_TEXT_BUF tb = tb_new();
         tb_cat(tb, qual);
         tb_replace(tb, '.', ':');
         return (r->Name = new_string(tb_get(tb)));
      }
      else if (is_design_unit(r->object.tree))
         return (r->Name = new_string(strrchr(qual, '.') + 1));
      else
         return (r->Name = new_string(qual));
   }

   c_abstractDecl *d = is_abstractDecl(obj);
   if (d != NULL && d->Name != NULL)
      return d->Name;
   else if (d != NULL)
      return (d->Name = new_string(istr(tree_ident(d->object.tree))));

   c_tool *t = is_tool(obj);
   if (t != NULL)
      return (vhpiStringT)PACKAGE_NAME;

   c_name *n = is_name(obj);
   if (n != NULL && n->Name != NULL)
      return n->Name;
   else if (n != NULL)
      should_not_reach_here();

   return NULL;
}

static vhpiStringT vhpi_get_full_name(c_vhpiObject *obj)
{
   c_subTypeDecl *std = is_subTypeDecl(obj);
   if (std != NULL && std->typeDecl.decl.FullName != NULL)
      return std->typeDecl.decl.FullName;
   else if (std != NULL && std->IsAnonymous) {
      c_typeDecl *base = vhpi_get_base_type(obj);
      assert(base != NULL);
      vhpiStringT fn = vhpi_get_full_name(&(base->decl.object));
      return (std->typeDecl.decl.FullName = fn);
   }

   c_abstractDecl *ad = is_abstractDecl(obj);
   if (ad != NULL && ad->FullName != NULL)
      return ad->FullName;
   else if (ad != NULL && ad->ImmRegion != NULL) {
      vhpiStringT prefix = vhpi_get_full_name(&ad->ImmRegion->object);
      const char *suffix = istr(tree_ident(ad->object.tree));
      return (ad->FullName = new_stringf("%s:%s", prefix, suffix));
   }

   c_abstractRegion *r = is_abstractRegion(obj);
   if (r != NULL && r->FullName != NULL)
      return r->FullName;
   else if (r != NULL) {
      vhpiStringT suffix = vhpi_get_name(obj);
      if (r->UpperRegion == NULL && is_package(r->object.tree))
         return (r->FullName = new_stringf("@%s", suffix));
      else if (r->UpperRegion == NULL)
         return (r->FullName = new_stringf(":%s", suffix));
      else {
         vhpiStringT prefix = vhpi_get_full_name(&(r->UpperRegion->object));
         return (r->FullName = new_stringf("%s:%s", prefix, suffix));
      }
   }

   c_name *n = is_name(obj);
   if (n != NULL && n->FullName != NULL)
      return n->FullName;
   else if (n != NULL)
      should_not_reach_here();

   return NULL;
}

static c_abstractRegion *vhpi_get_region(rt_scope_t *s)
{
   switch (s->kind) {
   case SCOPE_ROOT:
      return &(vhpi_context()->root)->designInstUnit.region;
   case SCOPE_INSTANCE:
      {
         c_abstractRegion *parent = vhpi_get_region(s->parent);
         if (parent == NULL)
            return NULL;
         else if (parent->object.tree == s->where)
            return parent;  // Root instance

         vhpiObjectListT *list =
            expand_lazy_list(&(parent->object), &(parent->Stmts));
         for (int i = 0; i < list->count; i++) {
            c_abstractRegion *r = is_abstractRegion(list->items[i]);
            if (r != NULL && r->object.tree == s->where)
               return r;
         }

         return NULL;
      }
   default:
      should_not_reach_here();
   }
}

static bool vhpi_name_cmp(c_vhpiObject *obj, const char *str)
{
   return strcasecmp((char *)vhpi_get_name(obj), str) == 0;
}

////////////////////////////////////////////////////////////////////////////////
// Public API

DLLEXPORT
int vhpi_release_handle(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(handle));

   vhpi_context_t *c = vhpi_context();
   handle_slot_t *slot = decode_handle(c, handle);
   if (slot == NULL) {
      vhpi_error(vhpiError, NULL, "invalid handle %p", handle);
      return 1;
   }
   else if (slot->kind == HANDLE_INTERNAL) {
      vhpi_error(vhpiError, obj_loc(slot->obj), "cannot release this handle as "
                 "it is owned by the system");
      return 1;
   }

   drop_handle(c, handle);
   return 0;
}

DLLEXPORT
int vhpi_compare_handles(vhpiHandleT handle1, vhpiHandleT handle2)
{
   vhpi_clear_error();

   VHPI_TRACE("vhpi_compare_handles handle1=%p handle2=%p", handle1, handle2);

   if (handle1 == handle2)
      return 1;

   vhpi_context_t *c = vhpi_context();
   handle_slot_t *slot1 = decode_handle(c, handle1);
   handle_slot_t *slot2 = decode_handle(c, handle2);

   return slot1 != NULL && slot2 != NULL && slot1->obj == slot2->obj;
}

DLLEXPORT
vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_datap_p=%s flags=%x", cb_data_pp(cb_data_p), flags);

   rt_model_t *m = vhpi_context()->model;

   switch (cb_data_p->reason) {
   case vhpiCbStartOfSimulation:
   case vhpiCbEndOfSimulation:
   case vhpiCbStartOfInitialization:
   case vhpiCbEndOfInitialization:
   case vhpiCbNextTimeStep:
   case vhpiCbRepNextTimeStep:
   case vhpiCbStartOfNextCycle:
   case vhpiCbRepStartOfNextCycle:
   case vhpiCbEndOfTimeStep:
   case vhpiCbRepEndOfTimeStep:
   case vhpiCbEndOfProcesses:
   case vhpiCbRepEndOfProcesses:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbStartOfAnalysis:
   case vhpiCbEndOfAnalysis:
   case vhpiCbStartOfTool:
   case vhpiCbEndOfTool:
   case vhpiCbStartOfElaboration:
   case vhpiCbEndOfElaboration:
      {
         c_callback *cb = recycle_object(sizeof(c_callback), vhpiCallbackK);
         init_callback(cb, cb_data_p, flags);

         if (cb->data.obj != NULL) {
            vhpi_error(vhpiWarning, NULL, "ignoring non-NULL obj field for %s",
                       vhpi_cb_reason_str(cb_data_p->reason));
            cb->data.obj = NULL;
         }

         if (cb->data.time != NULL) {
            vhpi_error(vhpiWarning, NULL, "ignoring non-NULL time field for %s",
                       vhpi_cb_reason_str(cb_data_p->reason));
            cb->data.time = NULL;
         }

         if (cb->data.value != NULL) {
            vhpi_error(vhpiWarning, NULL, "ignoring non-NULL value field for "
                       "%s", vhpi_cb_reason_str(cb_data_p->reason));
            cb->data.value = NULL;
         }

         cb->handle = internal_handle_for(&(cb->refcounted.object));
         APUSH(vhpi_context()->callbacks, cb->handle);

         if (flags & vhpiReturnCb)
            return user_handle_for(&(cb->refcounted.object));
         else
            return NULL;
      }

   case vhpiCbAfterDelay:
      {
         if (cb_data_p->time == NULL) {
            vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
            return NULL;
         }

         c_callback *cb = recycle_object(sizeof(c_callback), vhpiCallbackK);
         init_callback(cb, cb_data_p, flags);

         const uint64_t now = model_now(m, NULL);
         const uint64_t when = vhpi_time_to_native(cb_data_p->time) + now;

         cb->handle = internal_handle_for(&(cb->refcounted.object));
         model_set_timeout_cb(m, when, vhpi_global_cb, cb->handle);

         if (flags & vhpiReturnCb)
            return user_handle_for(&(cb->refcounted.object));
         else
            return NULL;
      }

   case vhpiCbValueChange:
      {
         c_vhpiObject *obj = from_handle(cb_data_p->obj);
         if (obj == NULL)
            return NULL;

         if (!vhpi_is_signal(obj)) {
            vhpi_error(vhpiInternal, obj_loc(obj), "cannot register value "
                       "callback for kind %s", vhpi_class_str(obj->kind));
            return NULL;
         }

         c_callback *cb = recycle_object(sizeof(c_callback), vhpiCallbackK);
         init_callback(cb, cb_data_p, flags);

         // LRM 08 section 23.29: [..] if the obj member of the callback
         // data structure contains a handle, the VHPI program may
         // release the handle after the vhpi_register_cb function
         // returns without affecting registration of the callback.
         cb->data.obj = internal_handle_for(obj);

         slice_list_t slices = AINIT;
         vhpi_collect_signals(obj, &slices);

         cb->handle = internal_handle_for(&(cb->refcounted.object));
         cb->watch = watch_new(m, vhpi_signal_event_cb, cb->handle,
                               WATCH_EVENT, slices.count);

         for (int i = 0; i < slices.count; i++)
            model_set_event_cb(m, slices.items[i].signal,
                               slices.items[i].offset,
                               slices.items[i].count,
                               cb->watch);

         ACLEAR(slices);

         if (flags & vhpiReturnCb)
            return user_handle_for(&(cb->refcounted.object));
         else
            return NULL;
      }

   default:
      vhpi_error(vhpiInternal, NULL, "unsupported reason %s",
                 vhpi_cb_reason_str(cb_data_p->reason));
      return NULL;
   }
}

DLLEXPORT
int vhpi_remove_cb(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(handle));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return 1;

   c_callback *cb = cast_callback(obj);
   if (cb == NULL)
      return 1;

   vhpi_context_t *c = vhpi_context();

   if (cb->Reason == vhpiCbValueChange) {
      watch_free(c->model, cb->watch);
      cb->watch = NULL;

      drop_handle(c, cb->data.obj);
   }

   cb->State = vhpiMature;

   drop_handle(c, cb->handle);   // Internal handle
   drop_handle(c, handle);       // User handle
   return 0;
}

DLLEXPORT
int vhpi_disable_cb(vhpiHandleT cb_obj)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_obj=%s", handle_pp(cb_obj));

   c_vhpiObject *obj = from_handle(cb_obj);
   if (obj == NULL)
      return 1;

   c_callback *cb = cast_callback(obj);
   if (cb == NULL)
      return 1;

   if (cb->State != vhpiEnable) {
      vhpi_error(vhpiWarning, obj_loc(obj),
                 "callback must be enabled in order to disable it");
      return 1;
   }

   cb->State = vhpiDisable;
   return 0;
}

DLLEXPORT
int vhpi_enable_cb(vhpiHandleT cb_obj)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_obj=%s", handle_pp(cb_obj));

   c_vhpiObject *obj = from_handle(cb_obj);
   if (obj == NULL)
      return 1;

   c_callback *cb = cast_callback(obj);
   if (cb == NULL)
      return 1;

   if (cb->State != vhpiDisable) {
      vhpi_error(vhpiWarning, obj_loc(obj),
                 "callback must be disabled in order to enable it");
      return 1;
   }

   cb->State = vhpiEnable;
   return 0;
}

DLLEXPORT
int vhpi_get_cb_info(vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
   vhpi_clear_error();

   VHPI_TRACE("object=%s", handle_pp(object));

   c_vhpiObject *obj = from_handle(object);
   if (obj == NULL)
      return 1;

   c_callback *cb = cast_callback(obj);
   if (cb == NULL)
      return 1;

   *cb_data_p = cb->data;
   return 0;
}

DLLEXPORT
vhpiHandleT vhpi_handle(vhpiOneToOneT type, vhpiHandleT referenceHandle)
{
   vhpi_clear_error();

   VHPI_TRACE("type=%s referenceHandle=%s", vhpi_one_to_one_str(type),
              handle_pp(referenceHandle));

   switch (type) {
   case vhpiRootInst:
      {
         c_rootInst *root = vhpi_context()->root;
         if (root == NULL) {
            vhpi_error(vhpiError, NULL, "cannot get root instance before "
                       "simulation has started");
            return NULL;
         }

         return user_handle_for(&root->designInstUnit.region.object);
      }
   case vhpiTool:
      {
         vhpi_context_t *c = vhpi_context();
         if (c->tool == NULL)
            c->tool = build_tool(0, NULL);

         return user_handle_for(&(c->tool->object));
      }
   default:
      break;
   }

   c_vhpiObject *obj = from_handle(referenceHandle);
   if (obj == NULL)
      return NULL;

   switch (type) {
   case vhpiBaseType:
      {
         c_typeDecl *td = vhpi_get_base_type(obj);
         if (td != NULL)
            return user_handle_for(&(td->decl.object));
      }
      break;
   case vhpiElemType:
      {
         c_typeDecl *td = vhpi_get_elem_type(obj);
         if (td != NULL)
            return user_handle_for(&(td->decl.object));
      }
      break;
   case vhpiValType:
      {
         c_typeDecl *td = vhpi_get_val_type(obj);
         if (td != NULL)
            return user_handle_for(&(td->decl.object));
      }
      break;
   case vhpiDesignUnit:
      {
         c_designInstUnit *iu = cast_designInstUnit(obj);
         if (iu == NULL)
            return NULL;

         if (iu->DesignUnit == NULL)
            return NULL;  // Unbound instance

         return user_handle_for(&(iu->DesignUnit->region.object));
      }
   case vhpiPrimaryUnit:
      {
         c_secondaryUnit *su = cast_secondaryUnit(obj);
         if (su == NULL)
            return NULL;

         return user_handle_for(&(su->PrimaryUnit->region.object));
      }
   case vhpiPrefix:
      {
         c_prefixedName *pn = is_prefixedName(obj);
         if (pn != NULL)
            return user_handle_for(pn->Prefix);
      }
      break;
   case vhpiSuffix:
      {
         c_selectedName *sn = is_selectedName(obj);
         if (sn != NULL)
            return user_handle_for(&(sn->Suffix->decl.object));
      }
      break;
   case vhpiType:
   case DEPRECATED_vhpiSubtype:
      {
         c_typeDecl *td = vhpi_get_type(obj);
         if (td != NULL)
            return user_handle_for(&(td->decl.object));
      }
      break;
   case vhpiParamDecl:
      {
         c_forGenerate *g = cast_forGenerate(obj);
         if (g == NULL)
            return NULL;

         return user_handle_for(&(g->ParamDecl->objDecl.decl.object));
      }
   case DEPRECATED_vhpiReturnTypeMark:
   case DEPRECATED_vhpiName:
   case DEPRECATED_vhpiTypeMark:
   case DEPRECATED_vhpiDecl:
      vhpi_error(vhpiError, obj_loc(obj), "relationship %s is deprecated and "
                 "not implemented in vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;
   default:
      vhpi_error(vhpiInternal, obj_loc(obj), "relationship %s not supported in "
                 "vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;
   }

   vhpi_error(vhpiError, obj_loc(obj), "invalid relationship %s for handle %s",
              vhpi_one_to_one_str(type), handle_pp(referenceHandle));
   return NULL;
}

DLLEXPORT
vhpiHandleT vhpi_handle_by_name(const char *name, vhpiHandleT scope)
{
   vhpi_clear_error();

   VHPI_TRACE("name=%s scope=%s", name, handle_pp(scope));

   char *copy LOCAL = xstrdup(name), *saveptr;
   char *elem = strtok_r(copy, ":.", &saveptr);

   c_vhpiObject *where = NULL;
   if (scope == NULL) {
      vhpi_context_t *c = vhpi_context();

      if (c->root == NULL) {
         vhpi_error(vhpiError, NULL, "design has not been elaborated");
         return NULL;
      }

      if (vhpi_name_cmp(&c->root->designInstUnit.region.object, elem))
         where = &(c->root->designInstUnit.region.object);
      else {
         for (int i = 0; i < c->packages.count; i++) {
            // TODO: not using vhpi_name_cmp here due to lib: prefix
            const char *full = istr(tree_ident(c->packages.items[i]->tree));
            const char *simple = strchr(full, '.') + 1;
            if (strcasecmp(simple, elem) == 0) {
               where = c->packages.items[i];
               break;
            }
         }

         if (where == NULL) {
            vhpi_error(vhpiError, NULL, "no design unit instance named %s",
                       elem);
            return NULL;
         }
      }

      elem = strtok_r(NULL, ":.", &saveptr);
   }
   else if ((where = from_handle(scope)) == NULL)
      return NULL;

   for (; elem != NULL; elem = strtok_r(NULL, ":.", &saveptr)) {
      bool found = false;
      c_iterator it = {};
      c_abstractRegion *region = is_abstractRegion(where);
      if (region != NULL) {
         vhpiObjectListT *decls = expand_lazy_list(where, &(region->Decls));
         for (int i = 0; i < decls->count; i++) {
            if (vhpi_name_cmp(decls->items[i], elem)) {
               where = decls->items[i];
               found = true;
               break;
            }
         }

         if (!found) {
            vhpiObjectListT *stmts = expand_lazy_list(where, &(region->Stmts));
            for (int i = 0; i < stmts->count; i++) {
               c_abstractRegion *r = is_abstractRegion(stmts->items[i]);
               if (r != NULL && vhpi_name_cmp(stmts->items[i], elem)) {
                  where = &(r->object);
                  found = true;
                  break;
               }
            }
         }
      }
      else if (init_iterator(&it, vhpiSelectedNames, where)) {
         for (int i = 0; i < it.list->count; i++) {
            c_selectedName *sn = is_selectedName(it.list->items[i]);
            assert(sn != NULL);

            if (vhpi_name_cmp(&sn->Suffix->decl.object, elem)) {
               where = &(sn->prefixedName.name.expr.object);
               found = true;
               break;
            }
         }
      }

      if (!found) {
         vhpi_error(vhpiError, obj_loc(where), "suffix %s not found in prefix "
                    "of class %s", elem, vhpi_class_str(where->kind));
         return NULL;
      }
   }

   return user_handle_for(where);
}

DLLEXPORT
vhpiHandleT vhpi_handle_by_index(vhpiOneToManyT itRel,
                                 vhpiHandleT parent,
                                 int32_t index)
{
   vhpi_clear_error();

   VHPI_TRACE("itRel=%s parent=%s index=%d", vhpi_one_to_many_str(itRel),
              handle_pp(parent), index);

   c_vhpiObject *obj = NULL;
   if (parent != NULL && (obj = from_handle(parent)) == NULL)
      return NULL;

   c_iterator it = {};
   if (!init_iterator(&it, itRel, obj)) {
      vhpi_error(vhpiError, obj_loc(obj),
                 "relation %s not supported for parent %s",
                 vhpi_one_to_many_str(itRel), handle_pp(parent));
      return NULL;
   }

   if (it.single ? index : index >= it.list->count) {
      vhpi_error(vhpiError, obj_loc(obj), "invalid %s index %d",
                 vhpi_one_to_many_str(itRel), index);
      return NULL;
   }

   return user_handle_for(it.single ?: it.list->items[index]);
}

DLLEXPORT
vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("type=%s handle=%s", vhpi_one_to_many_str(type),
              handle_pp(handle));

   c_vhpiObject *obj = NULL;
   if (handle != NULL && (obj = from_handle(handle)) == NULL)
      return NULL;

   c_iterator *it = recycle_object(sizeof(c_iterator), vhpiIteratorK);
   if (!init_iterator(it, type, obj)) {
      vhpi_error(vhpiError, obj_loc(obj),
                 "relation %s not supported for handle %s",
                 vhpi_one_to_many_str(type), handle_pp(handle));
      return NULL;
   }

   return user_handle_for(&(it->refcounted.object));
}

DLLEXPORT
vhpiHandleT vhpi_scan(vhpiHandleT iterator)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(iterator));

   c_vhpiObject *obj = from_handle(iterator);
   if (obj == NULL)
      return NULL;

   c_iterator *it = cast_iterator(obj);
   if (it == NULL)
      return NULL;

   if (it->single != NULL && it->pos++ > 0)
      goto end_of_iterator;
   else if (it->single != NULL)
      return user_handle_for(it->single);

   while (it->pos < it->list->count) {
      c_vhpiObject *obj = it->list->items[it->pos++];
      if (it->filter == NULL || (*it->filter)(obj) != NULL)
         return user_handle_for(obj);
   }

 end_of_iterator:
   // According to LRM 2008 section 23.33 the handle is released by
   // vhpi_scan when it reaches the end of the list
   drop_handle(vhpi_context(), iterator);

   return NULL;
}

DLLEXPORT
vhpiIntT vhpi_get(vhpiIntPropertyT property, vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              handle_pp(handle));

   if (handle == NULL) {
      switch (property) {
      case vhpiPhaseP:
         return vhpi_context()->phase;

      default:
         vhpi_error(vhpiFailure, NULL, "property %s cannot be used with NULL "
                    "handle", vhpi_property_str(property));
         return vhpiUndefined;
      }
   }

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return vhpiUndefined;

   switch (property) {
   case vhpiStateP:
      {
         c_callback *cb = cast_callback(obj);
         if (cb == NULL)
            goto missing_property;

         return cb->State;
      }
   case vhpiKindP:
      return obj->kind;
   case vhpiIsCompositeP:
   case vhpiIsScalarP:
      {
         c_typeDecl *td = is_typeDecl(obj);
         if (td == NULL)
            goto missing_property;

         if (property == vhpiIsCompositeP)
            return td->IsComposite;
         else
            return td->IsScalar;
      }
   case vhpiSizeP:
      {
         vhpiIntT size = vhpi_get_size(obj);
         if (size == vhpiUndefined)
            goto missing_property;

         return size;
      }
   case vhpiArgcP:
      {
         c_tool *t = is_tool(obj);
         if (t != NULL)
            return t->argv.count;

         goto missing_property;
      }
   case vhpiRandomSeedP:
      {
         c_tool *t = is_tool(obj);
         if (t != NULL)
            return opt_get_int(OPT_RANDOM_SEED);

         goto missing_property;
      }
   case vhpiNumDimensionsP:
      {
         c_typeDecl *td = vhpi_get_base_type(obj);
         if (td != NULL) {
            c_arrayTypeDecl *a = is_arrayTypeDecl(&(td->decl.object));
            if (a != NULL)
               return a->NumDimensions;
         }

         goto missing_property;
      }
   case vhpiIsUnconstrainedP:
      {
         c_range *r = is_range(obj);
         if (r != NULL)
            return r->IsUnconstrained;

         c_typeDecl *td = is_typeDecl(obj);
         if (td != NULL)
            return td->IsUnconstrained;

         goto missing_property;
      }
   case vhpiLeftBoundP:
   case vhpiRightBoundP:
      {
         c_intRange *ir = is_intRange(obj);
         if (ir == NULL)
            goto missing_property;

         if (!range_sync(ir)) {
            vhpi_error(vhpiError, obj_loc(obj), "cannot get bounds of "
                       "unconstrained range");
            return vhpiUndefined;
         }
         else if (property == vhpiLeftBoundP)
            return ir->LeftBound;
         else
            return ir->RightBound;
      }
   case vhpiIsUpP:
   case vhpiIsNullP:
   case vhpiIsDiscreteP:
      {
         c_range *r = is_range(obj);
         if (r == NULL)
            goto missing_property;

         bool unconstrained = r->IsUnconstrained;
         c_intRange *ir = is_intRange(obj);
         if (ir != NULL && range_sync(ir))
            unconstrained = false;

         if (unconstrained) {
            vhpi_error(vhpiError, obj_loc(obj), "cannot get bounds of "
                       "unconstrained range");
            return vhpiUndefined;
         }

         if (property == vhpiIsUpP)
            return r->IsUp;
         else if (property == vhpiIsNullP)
            return r->IsNull;
         else
            return r->IsDiscrete;
      }
   case vhpiStaticnessP:
      {
         c_objDecl *decl = is_objDecl(obj);
         if (decl != NULL)
            return decl->Staticness;

         c_enumLiteral *el = is_enumLiteral(obj);
         if (el != NULL)
            return vhpiGloballyStatic;

         c_expr *e = cast_expr(obj);
         if (e != NULL)
            return e->Staticness;

         goto missing_property;
      }
   case vhpiBaseIndexP:
      {
         c_indexedName *in = is_indexedName(obj);
         if (in != NULL)
            return in->BaseIndex;

         goto missing_property;
      }
   case vhpiNumLiteralsP:
      {
         c_enumTypeDecl *etd = cast_enumTypeDecl(obj);
         if (etd == NULL)
            goto missing_property;

         return expand_lazy_list(obj, &(etd->EnumLiterals))->count;
      }
   case vhpiPositionP:
      {
         c_elemDecl *ed = is_elemDecl(obj);
         if (ed != NULL)
            return ed->Position;

         c_enumLiteral *el = is_enumLiteral(obj);
         if (el != NULL)
            return el->Position;

         goto missing_property;
      }
   case vhpiNumFieldsP:
      {
         c_recordTypeDecl *rtd = cast_recordTypeDecl(obj);
         if (rtd != NULL)
            return expand_lazy_list(obj, &rtd->RecordElems)->count;

         goto missing_property;
      }
   case vhpiModeP:
      {
         c_genericDecl *gd = is_genericDecl(obj);
         if (gd != NULL)
            return gd->Mode;

         c_portDecl *pd = is_portDecl(obj);
         if (pd != NULL)
            return pd->Mode;

         goto missing_property;
      }
   case vhpiIsLocalP:
      {
         c_genericDecl *gd = is_genericDecl(obj);
         if (gd == NULL)
            goto missing_property;

         return gd->IsLocal;
      }
   case vhpiIsGuardedP:
      {
         c_blockStmt *bs = is_blockStmt(obj);
         if (bs == NULL)
            goto missing_property;

         return bs->IsGuarded;
      }
   case vhpiIsSeqStmtP:
      {
         c_stmt *s = is_stmt(obj);
         if (s == NULL)
            goto missing_property;

         return s->IsSeqStmt;
      }
   case vhpiGenerateIndexP:
      {
         c_forGenerate *g = is_forGenerate(obj);
         if (g == NULL)
            goto missing_property;

         return g->GenerateIndex;
      }
   case vhpiCapabilitiesP:
      {
         c_tool *tool = is_tool(obj);
         if (tool == NULL)
            goto missing_property;

         return tool->capabilities;
      }
   default:
      vhpi_error(vhpiFailure, obj_loc(obj), "unsupported property %s in "
                 "vhpi_get", vhpi_property_str(property));
      return vhpiUndefined;
   }

missing_property:
   vhpi_error(vhpiError, obj_loc(obj), "object does not have property %s",
              vhpi_property_str(property));
   return vhpiUndefined;
}

DLLEXPORT
const vhpiCharT *vhpi_get_str(vhpiStrPropertyT property, vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              handle_pp(handle));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return NULL;

   switch (property) {
   case vhpiKindStrP:
      return (vhpiCharT *)vhpi_class_str(obj->kind);
   case vhpiNameP:
   case vhpiCaseNameP:
      {
         vhpiCharT *str = vhpi_get_name(obj);
         if (str != NULL)
            return str;

         goto missing_property;
      }
   case vhpiFullNameP:
   case vhpiFullCaseNameP:
      {
         vhpiCharT *str = vhpi_get_full_name(obj);
         if (str != NULL)
            return str;

         goto missing_property;
      }
   case vhpiToolVersionP:
      {
         c_tool *t = is_tool(obj);
         if (t != NULL)
            return (vhpiCharT *)PACKAGE_VERSION;

         goto missing_property;
      }
   case vhpiUnitNameP:
      {
         c_designUnit *du = is_designUnit(obj);
         if (du != NULL)
            return du->UnitName;

         goto missing_property;
      }
   case vhpiStrValP:
      {
         c_argv *arg = is_argv(obj);
         if (arg != NULL)
            return arg->StrVal;

         c_enumLiteral *el = is_enumLiteral(obj);
         if (el != NULL)
            return vhpi_get_name(obj);

         goto missing_property;
      }
   case vhpiLabelNameP:
      {
         c_stmt *s = is_stmt(obj);
         if (s != NULL)
            return s->LabelName;

         goto missing_property;
      }
   case vhpiFileNameP:
      {
         c_abstractRegion *region = is_abstractRegion(obj);
         if (region != NULL)
            return (vhpiCharT *)loc_file_str(obj_loc(&(region->object)));

         c_abstractDecl *d = is_abstractDecl(obj);
         if (d != NULL)
            return (vhpiCharT *)loc_file_str(obj_loc(&(d->object)));

         goto missing_property;
      }
   default:
      vhpi_error(vhpiFailure, obj_loc(obj), "unsupported property %s in "
                 "vhpi_get_str", vhpi_property_str(property));
      return NULL;
   }

missing_property:
   vhpi_error(vhpiError, obj_loc(obj), "object does not have string property "
              "%s", vhpi_property_str(property));
   return NULL;
}

DLLEXPORT
vhpiRealT vhpi_get_real(vhpiRealPropertyT property,
                        vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              handle_pp(handle));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return NAN;

   switch (property) {
   case vhpiFloatLeftBoundP:
   case vhpiFloatRightBoundP:
      {
         c_floatRange *fr = cast_floatRange(obj);
         if (fr == NULL)
            return NAN;

         if (property == vhpiFloatLeftBoundP)
            return fr->FloatLeftBound;
         else
            return fr->FloatRightBound;
      }

   default:
      vhpi_error(vhpiError, obj_loc(obj), "invalid real property %s",
                 vhpi_property_str(property));
   }

   return NAN;
}

DLLEXPORT
vhpiPhysT vhpi_get_phys(vhpiPhysPropertyT property,
                        vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              handle_pp(handle));

   const vhpiPhysT invalid = { 0, 0 };

   if (property == vhpiResolutionLimitP)
      return vhpiFS;

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return invalid;

   switch (property) {
   case vhpiPhysValP:
      {
         c_objDecl *decl = cast_objDecl(obj);
         if (decl == NULL)
            return invalid;

         c_typeDecl *t = vhpi_get_base_type(obj);
         c_physTypeDecl *td = cast_physTypeDecl(&(t->decl.object));
         if (td == NULL)
            return invalid;

         vhpi_ptr_t ptr = vhpi_get_ptr(obj);
         if (ptr.kind == PTR_NULL)
            return invalid;

         uint64_t value = 0;
#define READ_PHYS(type) value = ((const type *)ptr.data)[ptr.offset]
         FOR_ALL_SIZES(t->elem_bytes, READ_PHYS);

         return vhpi_phys_from_native(value);
      }
   case vhpiPhysLeftBoundP:
   case vhpiPhysRightBoundP:
      {
         c_physRange *pr = cast_physRange(obj);
         if (pr == NULL)
            return invalid;

         if (property == vhpiPhysLeftBoundP)
            return pr->PhysLeftBound;
         else
            return pr->PhysRightBound;
      }
   case vhpiPhysPositionP:
      {
         c_unitDecl *ud = cast_unitDecl(obj);
         if (ud == NULL)
            return invalid;

         return ud->PhysPosition;
      }
   default:
      vhpi_error(vhpiError, obj_loc(obj), "invalid physical property %s",
                 vhpi_property_str(property));
   }

   return invalid;
}

static bool vhpi_scalar_fits_format(vhpiFormatT format, int size)
{
   // Allow reading integral scalar values with a wider type
   switch (format) {
   case vhpiLogicVal:
   case vhpiSmallEnumVal:
   case vhpiCharVal:
      return size == 1;
   case vhpiEnumVal:
      return size <= 4;
   case vhpiIntVal:
      return size <= 4;
   case vhpiLongIntVal:
      return size <= 8;
   default:
      return false;
   }
}

static int vhpi_to_string(c_typeDecl *td, int64_t scalar, int num_elems,
                          const void *ptr, uint32_t offset, vhpiValueT *value_p)
{
   switch (td->format) {
   case vhpiSmallEnumVal:
   case vhpiEnumVecVal:
      {
         c_enumTypeDecl *etd = is_enumTypeDecl(&td->decl.object);
         assert(etd != NULL);

         vhpiObjectListT *lits =
            expand_lazy_list(&etd->scalar.typeDecl.decl.object,
                             &etd->EnumLiterals);

         if (scalar < 0 || scalar >= lits->count) {
            vhpi_error(vhpiError, NULL, "enumeration value %"PRIi64
                       " out of range", scalar);
            return -1;
         }

         c_enumLiteral *lit = is_enumLiteral(lits->items[scalar]);
         assert(lit != NULL);

         ident_t name = tree_ident(lit->decl.object.tree);

         const size_t len = ident_len(name) + 1;
         if (len > value_p->bufSize)
            return len;

         memcpy(value_p->value.str, istr(name), len);
         return 0;
      }
   case vhpiLogicVal:
   case vhpiLogicVecVal:
      {
         if (value_p->bufSize < num_elems + 1)
            return num_elems + 1;

         value_p->numElems = num_elems;

         const vhpiCharT *p = ptr + offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.str[i] = td->map_str[*p++];

         value_p->value.str[value_p->numElems] = '\0';
         return 0;
      }
   case vhpiStrVal:
      {
         if (value_p->bufSize < num_elems + 1)
            return num_elems + 1;

         value_p->numElems = num_elems;

         const vhpiCharT *p = ptr + offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.str[i] = *p++;

         value_p->value.str[value_p->numElems] = '\0';
         return 0;
      }

   default:
      vhpi_error(vhpiError, NULL, "cannot format %s as string",
                 type_pp(td->type));
      return -1;
   }
}

DLLEXPORT
int vhpi_get_value(vhpiHandleT expr, vhpiValueT *value_p)
{
   vhpi_clear_error();

   VHPI_TRACE("expr=%s value_p=%p", handle_pp(expr), value_p);

   c_vhpiObject *obj = from_handle(expr);
   if (obj == NULL)
      return -1;

   vhpi_ptr_t ptr = vhpi_get_ptr(obj);
   VHPI_DEBUG("ptr kind=%d data=%p offset=%d", ptr.kind, ptr.data, ptr.offset);

   if (ptr.kind == PTR_NULL) {
      vhpi_error(vhpiError, obj_loc(obj), "cannot get value of %s",
                 vhpi_class_str(obj->kind));
      return -1;
   }

   c_typeDecl *td = vhpi_get_type(obj);
   assert(td != NULL);

   if (value_p->format == vhpiObjTypeVal)
      value_p->format = td->format;
   else if (value_p->format == vhpiBinStrVal && td->map_str != NULL)
      ;
   else if (value_p->format != td->format && value_p->format != vhpiStrVal
            && !vhpi_scalar_fits_format(value_p->format, td->elem_bytes)) {
      vhpi_error(vhpiError, obj_loc(obj), "invalid format %s for object %s: "
                 "expecting %s", vhpi_format_str(value_p->format),
                 vhpi_get_name(obj), vhpi_format_str(td->format));
      return -1;
   }

   const vhpiIntT num_elems = vhpi_get_size(obj);
   if (num_elems == vhpiUndefined) {
      vhpi_error(vhpiError, obj_loc(obj), "cannot get size of %s",
                 vhpi_class_str(obj->kind));
      return -1;
   }

   int64_t scalar = 0;
   switch (td->format) {
   case vhpiLogicVal:
   case vhpiSmallEnumVal:
   case vhpiCharVal:
      scalar = ((const vhpiCharT *)ptr.data)[ptr.offset];
      break;
   case vhpiEnumVal:
#define READ_ENUM(type) scalar = ((const type *)ptr.data)[ptr.offset]
      FOR_ALL_SIZES(td->elem_bytes, READ_ENUM);
      break;
   case vhpiIntVal:
      scalar = ((const vhpiIntT *)ptr.data)[ptr.offset];
      break;
   case vhpiLongIntVal:
      scalar = ((const vhpiLongIntT *)ptr.data)[ptr.offset];
      break;
   default:
      break;
   }

   switch (value_p->format) {
   case vhpiLogicVal:
      value_p->numElems = num_elems;
      value_p->value.enumv = scalar;
      return 0;
   case vhpiSmallEnumVal:
      value_p->numElems = num_elems;
      value_p->value.smallenumv = scalar;
      return 0;
   case vhpiEnumVal:
      value_p->numElems = num_elems;
      value_p->value.enumv = scalar;
      return 0;
   case vhpiCharVal:
      value_p->numElems = num_elems;
      value_p->value.ch = scalar;
      return 0;
   case vhpiIntVal:
      value_p->numElems = num_elems;
      value_p->value.intg = scalar;
      return 0;
   case vhpiLongIntVal:
      value_p->numElems = num_elems;
      value_p->value.longintg = scalar;
      return 0;
   case vhpiRealVal:
      value_p->numElems = num_elems;
      value_p->value.real = ((const double *)ptr.data)[ptr.offset];
      return 0;
   case vhpiTimeVal:
   case vhpiPhysVal:
#define READ_ENUM(type) scalar = ((const type *)ptr.data)[ptr.offset]
      FOR_ALL_SIZES(td->elem_bytes, READ_ENUM);
      value_p->numElems = num_elems;
      value_p->value.phys = vhpi_phys_from_native(scalar);
      return 0;
   case vhpiLogicVecVal:
      {
         const int max = value_p->bufSize / sizeof(vhpiEnumT);
         if (max < num_elems)
            return num_elems * sizeof(vhpiEnumT);

         value_p->numElems = num_elems;

         const uint8_t *p = ptr.data + ptr.offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.enumvs[i] = *p++;

         return 0;
      }
   case vhpiSmallEnumVecVal:
      {
         const int max = value_p->bufSize / sizeof(vhpiSmallEnumT);
         if (max < num_elems)
            return num_elems * sizeof(vhpiSmallEnumT);

         value_p->numElems = num_elems;

         const uint8_t *p = ptr.data + ptr.offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.smallenumvs[i] = *p++;

         return 0;
      }
   case vhpiEnumVecVal:
      {
         const int max = value_p->bufSize / sizeof(vhpiEnumT);
         if (max < num_elems)
            return num_elems * sizeof(vhpiEnumT);

         value_p->numElems = num_elems;

#define READ_ENUMV(type) do {                                         \
            const type *p = ((const type *)ptr.data) + ptr.offset;    \
            for (int i = 0; i < value_p->numElems; i++)               \
               value_p->value.enumvs[i] = *p++;                       \
         } while (0)

         FOR_ALL_SIZES(td->elem_bytes, READ_ENUMV);
         return 0;
      }
   case vhpiTimeVecVal:
   case vhpiPhysVecVal:
      {
         const int max = value_p->bufSize / sizeof(vhpiPhysT);
         if (max < num_elems)
            return num_elems * sizeof(vhpiPhysT);

         value_p->numElems = num_elems;

#define READ_PHYSV(type) do {                                           \
            const type *p = ((const type *)ptr.data) + ptr.offset;      \
            for (int i = 0; i < value_p->numElems; i++)                 \
               value_p->value.physs[i] = vhpi_phys_from_native(*p++);   \
         } while (0)

         FOR_ALL_SIZES(td->elem_bytes, READ_PHYSV);
         return 0;
      }
   case vhpiStrVal:
      return vhpi_to_string(td, scalar, num_elems, ptr.data,
                            ptr.offset, value_p);
   case vhpiBinStrVal:
      {
         if (value_p->bufSize < num_elems + 1)
            return num_elems + 1;

         value_p->numElems = num_elems;

         const vhpiCharT *p = ptr.data + ptr.offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.str[i] = td->map_str[*p++];

         value_p->value.str[value_p->numElems] = '\0';
         return 0;
      }
   case vhpiRealVecVal:
      {
         if (value_p->bufSize / sizeof(vhpiRealT) < num_elems)
            return num_elems * sizeof(vhpiRealT);

         value_p->numElems = num_elems;

         const double *p = ((const double *)ptr.data) + ptr.offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.reals[i] = *p++;

         return 0;
      }
   case vhpiIntVecVal:
      {
         if (value_p->bufSize / sizeof(vhpiIntT) < num_elems)
            return num_elems * sizeof(vhpiIntT);

         value_p->numElems = num_elems;

#define READ_INTGS(type) do {                                         \
            const type *p = ((const type *)ptr.data) + ptr.offset;    \
            for (int i = 0; i < value_p->numElems; i++)               \
               value_p->value.intgs[i] = *p++;                        \
         } while (0)

         FOR_ALL_SIZES(td->elem_bytes, READ_INTGS);

         return 0;
      }
   default:
      vhpi_error(vhpiError, obj_loc(obj), "unsupported format %s",
                 vhpi_format_str(value_p->format));
      return -1;
   }
}

static void *vhpi_from_string(c_typeDecl *td, const vhpiValueT *value_p,
                              int *num_elems)
{
   switch (td->format) {
   case vhpiStrVal:
      {
         *num_elems = value_p->bufSize - 1;
         vhpiCharT *mem = xmalloc(*num_elems);
         for (int i = 0; i < *num_elems; i++)
            mem[i] = value_p->value.str[i];
         return mem;
      }

   case vhpiIntVal:
   case vhpiLongIntVal:
      {
         *num_elems = 1;
         vhpiLongIntT *mem = xmalloc(sizeof(vhpiLongIntT));
         char *eptr = NULL;
         *mem = strtoll((char *)value_p->value.str, &eptr, 0);
         if (*eptr != '\0') {
            vhpi_error(vhpiError, NULL, "invalid integer '%s'",
                       value_p->value.str);
            free(mem);
            return NULL;
         }
         return mem;
      }

   default:
      vhpi_error(vhpiError, NULL, "cannot parse %pT as string", td->type);
      return NULL;
   }
}

DLLEXPORT
int vhpi_put_value(vhpiHandleT handle,
                   vhpiValueT *value_p,
                   vhpiPutValueModeT mode)
{
   // See LRM 2008 section 22.5.3 for discussion of semantics

   vhpi_clear_error();

   VHPI_TRACE("handle=%s value_p=%p mode=%s", handle_pp(handle), value_p,
              vhpi_put_value_mode_str(mode));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return 1;

   vhpi_ptr_t ptr = vhpi_get_ptr(obj);
   if (ptr.kind == PTR_NULL) {
      vhpi_error(vhpiFailure, obj_loc(obj), "cannot change value of %s",
                 vhpi_class_str(obj->kind));
      return 1;
   }

   c_typeDecl *td = vhpi_get_type(obj);
   assert(td != NULL);

   if (mode == vhpiSizeConstraint) {
      c_funcDecl *fd = is_funcDecl(obj);
      if (fd == NULL) {
         vhpi_error(vhpiError, obj_loc(obj), "vhpiSizeConstraint is only valid "
                    "for function result");
         return 1;
      }
      else if (!td->IsUnconstrained) {
         vhpi_error(vhpiError, obj_loc(obj), "function return type does not "
                    "have a size constraint");
         return 1;
      }

      fd->size = value_p->numElems;

      vhpi_context_t *c = vhpi_context();
      c->args[0].pointer = tlab_alloc(c->tlab, fd->size * td->elem_bytes);
      c->args[1].integer = 1;
      c->args[2].integer = fd->size;

      return 0;
   }

   void *ext LOCAL = NULL, *src = NULL;
   union {
      uint8_t  int8_t_val;
      uint16_t int16_t_val;
      uint32_t int32_t_val;
      int64_t  int64_t_val;
      double   double_val;
   } scalar = { .int64_t_val = 0 };
   int num_elems = 0;

   switch (value_p->format) {
   case vhpiLogicVal:
      num_elems = 1;
      scalar.int8_t_val = value_p->value.enumv;
      src = &scalar;
      break;

   case vhpiSmallEnumVal:
      num_elems = 1;
      scalar.int8_t_val = value_p->value.smallenumv;
      src = &scalar;
      break;

   case vhpiEnumVal:
      num_elems = 1;
      src = &scalar;

#define STORE_ENUM(type) do {                            \
         scalar.type##_val = value_p->value.enumv;       \
      } while (0)

      FOR_ALL_SIZES(td->elem_bytes, STORE_ENUM);
      break;

   case vhpiCharVal:
      num_elems = 1;
      scalar.int8_t_val = value_p->value.ch;
      src = &scalar;
      break;

   case vhpiIntVal:
      num_elems = 1;
      scalar.int64_t_val = value_p->value.intg;
      src = &scalar;   // Assume little endian
      break;

   case vhpiRealVal:
      num_elems = 1;
      scalar.double_val = value_p->value.real;
      src = &scalar;
      break;

   case vhpiLogicVecVal:
      num_elems = value_p->bufSize / sizeof(vhpiEnumT);
      ext = src = xmalloc(num_elems);
      for (int i = 0; i < num_elems; i++)
         ((uint8_t *)ext)[i] = value_p->value.enumvs[i];
      break;

   case vhpiSmallEnumVecVal:
      num_elems = value_p->bufSize / sizeof(vhpiSmallEnumT);
      ext = src = xmalloc(num_elems);
      for (int i = 0; i < num_elems; i++)
         ((uint8_t *)ext)[i] = value_p->value.smallenumvs[i];
      break;

   case vhpiEnumVecVal:
      {
         num_elems = value_p->bufSize / sizeof(vhpiEnumT);
         ext = src = xmalloc_array(num_elems, td->elem_bytes);

#define STORE_ENUMV(type) do {                                  \
            for (int i = 0; i < num_elems; i++)                 \
               ((type *)ext)[i] = value_p->value.enumvs[i];     \
         } while (0)

         FOR_ALL_SIZES(td->elem_bytes, STORE_ENUMV);
         break;
      }

   case vhpiStrVal:
      if ((ext = src = vhpi_from_string(td, value_p, &num_elems)) == NULL)
         return 1;
      break;

   case vhpiRealVecVal:
      {
         num_elems = value_p->bufSize / sizeof(vhpiRealT);
         ext = src = xmalloc_array(num_elems, sizeof(double));
         for (int i = 0; i < num_elems; i++)
            ((double *)ext)[i] = value_p->value.reals[i];
         break;
      }

   case vhpiIntVecVal:
      {
         num_elems = value_p->bufSize / sizeof(vhpiIntT);
         ext = src = xmalloc_array(num_elems, sizeof(vhpiIntT));

#define STORE_INTGS(type) do {                                          \
            for (int i = 0; i < num_elems; i++)                         \
               ((type *)ext)[i] = (int64_t)value_p->value.intgs[i];     \
         } while (0)

         FOR_ALL_SIZES(td->elem_bytes, STORE_INTGS);
         break;
      }

   default:
      vhpi_error(vhpiFailure, obj_loc(obj), "value format %d not supported "
                 "in vhpi_put_value", value_p->format);
      return 1;
   }

   const vhpiIntT size = vhpi_get_size(obj);
   if (size == vhpiUndefined) {
      vhpi_error(vhpiError, obj_loc(obj), "cannot get size of %s object",
                 vhpi_class_str(obj->kind));
      return 1;
   }
   else if (num_elems > size) {
      vhpi_error(vhpiError, obj_loc(obj), "too many values (%d) for "
                 "%s object with %d elements", num_elems,
                 vhpi_class_str(obj->kind), size);
      return 1;
   }

   if (ptr.kind == PTR_SIGNAL) {
      rt_model_t *model = vhpi_context()->model;
      if (!model_can_create_delta(model)) {
         vhpi_error(vhpiError, obj_loc(obj), "cannot create delta cycle "
                    "during current simulation phase");
         return 1;
      }

      switch (mode) {
      case vhpiForcePropagate:
         force_signal(model, ptr.signal, src, ptr.offset, num_elems);
         return 0;
      case vhpiDepositPropagate:
         sched_deposit(model, ptr.signal, src, ptr.offset, num_elems, 0, false);
         return 0;
      case vhpiRelease:
         release_signal(model, ptr.signal, ptr.offset, num_elems);
         return 0;
      default:
         goto unsupported;
      }
   }

   switch (obj->kind) {
   case vhpiFuncDeclK:
   case vhpiVarParamDeclK:
      {
         switch (mode) {
         case vhpiForce:
         case vhpiForcePropagate:
         case vhpiDeposit:
         case vhpiDepositPropagate:
            if (td->IsScalar)
               *(int64_t *)ptr.data = scalar.int64_t_val;
            else
               memcpy(ptr.data, ext, num_elems * td->elem_bytes);
            return 0;
         case vhpiRelease:
            return 0;   // Specified to have no effect
         default:
            break;
         }
      }
      break;
   default:
      goto unsupported;
   }

 unsupported:
   vhpi_error(vhpiFailure, obj_loc(obj), "mode %s not supported for %s in "
              "vhpi_put_value", vhpi_put_value_mode_str(mode),
              vhpi_class_str(obj->kind));
   return 1;
}

DLLEXPORT
int vhpi_protected_call(vhpiHandleT varHdl,
                        vhpiUserFctT userFct,
                        void *userData)
{
   vhpi_clear_error();

   VHPI_TRACE("varHdl=%s userFct=%p userData=%p", handle_pp(varHdl), userFct,
              userData);

   VHPI_MISSING;
}

DLLEXPORT
int vhpi_schedule_transaction(vhpiHandleT drivHdl,
                              vhpiValueT *value_p,
                              uint32_t numValues,
                              vhpiTimeT *delayp,
                              vhpiDelayModeT delayMode,
                              vhpiTimeT *pulseRejp)
{
   vhpi_clear_error();

   VHPI_TRACE("drivHdl=%s value_p=%p numValues=%u delayp=%p delayMode=%d "
              "pulseRejp=%p", handle_pp(drivHdl), value_p, numValues, delayp,
              delayMode, pulseRejp);

   VHPI_MISSING;
}

DLLEXPORT
int vhpi_format_value(const vhpiValueT *in_value_p,
                      vhpiValueT *out_value_p)
{
   vhpi_clear_error();

   VHPI_TRACE("in_value_p=%p out_value_p=%p", in_value_p, out_value_p);

   VHPI_MISSING;
}

DLLEXPORT
void vhpi_get_time(vhpiTimeT *time_p, long *cycles)
{
   vhpi_clear_error();

   VHPI_TRACE("time_p=%p cycles=%p", time_p, cycles);

   unsigned deltas;
   const uint64_t now = model_now(vhpi_context()->model, &deltas);

   if (time_p != NULL) {
      time_p->high = now >> 32;
      time_p->low  = now & 0xffffffff;
   }

   if (cycles != NULL)
      *cycles = deltas;
}

DLLEXPORT
int vhpi_get_next_time(vhpiTimeT *time_p)
{
   vhpi_clear_error();

   VHPI_TRACE("time_p=%p", time_p);

   const uint64_t next = model_next_time(vhpi_context()->model);
   if (next == TIME_HIGH)
      return vhpiNoActivity;

   time_p->high = next >> 32;
   time_p->low  = next & 0xffffffff;

   return 0;
}

DLLEXPORT
int vhpi_control(vhpiSimControlT command, ...)
{
   vhpi_clear_error();

   VHPI_TRACE("command=%d", command);

   switch (command) {
   case vhpiFinish:
   case vhpiStop:
      notef("VHPI plugin requested end of simulation");
      model_stop(vhpi_context()->model);
      return 0;

   case vhpiReset:
      vhpi_error(vhpiFailure, NULL, "vhpiReset not supported");
      return 1;

   default:
      vhpi_error(vhpiFailure, NULL, "unsupported command in vhpi_control");
      return 1;
   }
}

DLLEXPORT
vhpiHandleT vhpi_create(vhpiClassKindT kind,
                        vhpiHandleT handle1,
                        vhpiHandleT handle2)
{
   vhpi_clear_error();

   VHPI_TRACE("kind=%s handle1=%s handle2=%s", vhpi_class_str(kind),
              handle_pp(handle1), handle_pp(handle2));

   VHPI_MISSING;
}

DLLEXPORT
int vhpi_get_foreignf_info(vhpiHandleT handle, vhpiForeignDataT *foreignDatap)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(handle));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return 1;

   c_foreignf *f = cast_foreignf(obj);
   if (f == NULL)
      return 1;

   *foreignDatap = f->data;
   return 0;
}

DLLEXPORT
vhpiHandleT vhpi_register_foreignf(vhpiForeignDataT *foreignDatap)
{
   vhpi_clear_error();

   VHPI_TRACE("kind=%d libraryName=%s modelName=%s", foreignDatap->kind,
              foreignDatap->libraryName, foreignDatap->modelName);

   switch (foreignDatap->kind) {
   case vhpiFuncF:
   case vhpiProcF:
      {
         c_foreignf *f = new_object(sizeof(c_foreignf), vhpiForeignfK);
         f->data = *foreignDatap;

         // Make a defensive copy of the passed-in strings
         f->data.libraryName = (char *)new_string(foreignDatap->libraryName);
         f->data.modelName = (char *)new_string(foreignDatap->modelName);

         vhpi_context_t *c = vhpi_context();
         APUSH(c->foreignfs, &(f->object));

         return user_handle_for(&(f->object));
      }

   default:
      vhpi_error(vhpiInternal, NULL, "foreign model kind not supported");
      return NULL;
   }
}

DLLEXPORT
size_t vhpi_get_data(int32_t id, void *dataLoc, size_t numBytes)
{
   vhpi_clear_error();

   VHPI_TRACE("id=%d dataLoc=%p numBytes=%zu", id, dataLoc, numBytes);

   VHPI_MISSING;
}

DLLEXPORT
size_t vhpi_put_data(int32_t id, void *dataLoc, size_t numBytes)
{
   vhpi_clear_error();

   VHPI_TRACE("id=%d dataLoc=%p numBytes=%zu", id, dataLoc, numBytes);

   VHPI_MISSING;
}

////////////////////////////////////////////////////////////////////////////////
// Model construction

#if 0
static c_literal *build_literal(tree_t t)
{
   switch (tree_subkind(t)) {
   case L_INT:
      {
         c_intLiteral *l = new_object(sizeof(c_intLiteral), vhpiPhysLiteralK);
         return &(l->literal);
      }
   case L_PHYSICAL:
      {
         c_physLiteral *l = new_object(sizeof(c_literal), vhpiPhysLiteralK);
         return &(l->literal);
      }
   default:
      fatal_trace("cannot build VHPI object for literal kind %d",
                  tree_subkind(t));
   }
}

static c_expr *build_expr(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      return &(build_literal(t)->expr);
   default:
      fatal_trace("cannot build VHPI expr for tree kind %s",
                  tree_kind_str(tree_kind(t)));
   }
}
#endif

static c_physRange *build_phys_range(tree_t t)
{
   const int64_t left = assume_int(tree_left(t));
   const int64_t right = assume_int(tree_right(t));
   const range_kind_t dir = tree_subkind(t) == RANGE_TO;
   const bool null = dir == RANGE_TO ? left > right : right > left;

   c_physRange *pr = new_object(sizeof(c_physRange), vhpiPhysRangeK);
   init_range(&(pr->range), dir, null, true);

   pr->PhysLeftBound  = vhpi_phys_from_native(left);
   pr->PhysRightBound = vhpi_phys_from_native(right);

   return pr;
}

static c_floatRange *build_float_range(tree_t r)
{
   double left;
   if (!folded_real(tree_left(r), &left))
      left = -INFINITY;

   double right;
   if (!folded_real(tree_right(r), &right))
      right = +INFINITY;

   const range_kind_t dir = tree_subkind(r) == RANGE_TO;
   const bool null = dir == RANGE_TO ? left > right : right > left;

   c_floatRange *fr = new_object(sizeof(c_floatRange), vhpiFloatRangeK);
   init_range(&(fr->range), dir, null, true);

   fr->FloatLeftBound  = left;
   fr->FloatRightBound = right;

   return fr;
}

static c_intRange *build_int_range(tree_t r)
{
   range_kind_t dir = RANGE_TO;
   int64_t low, high, left = 1, right = 0;
   if (folded_bounds(r, &low, &high)) {
      dir = tree_subkind(r);
      left = (dir == RANGE_TO) ? low : high;
      right = (dir == RANGE_TO) ? high : low;
   }
   else
      vhpi_error(vhpiInternal, tree_loc(r), "cannot get bounds for range");

   const bool null = dir == RANGE_TO ? left > right : right > left;

   c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
   init_range(&(ir->range), dir == RANGE_TO, null, true);

   ir->LeftBound  = left;
   ir->RightBound = right;

   return ir;
}

static c_intRange *build_unconstrained(c_vhpiObject *source, int nth,
                                       bool IsDiscrete)
{
   c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
   ir->range.IsDiscrete = IsDiscrete;
   ir->range.IsUnconstrained = vhpiTrue;
   ir->source = source;
   ir->nth_dim = nth;
   return ir;
}

static c_typeDecl *build_arrayTypeDecl(type_t type, tree_t decl,
                                       c_abstractRegion *region)
{
   c_arrayTypeDecl *td =
      new_object(sizeof(c_arrayTypeDecl), vhpiArrayTypeDeclK);
   init_compositeTypeDecl(&(td->composite), decl, type, region);

   td->NumDimensions = dimension_of(type);

   c_vhpiObject *tobj = &(td->composite.typeDecl.decl.object);
   vhpi_list_add(&region->Decls.list, tobj);

   type_t elem = type_elem(type);
   if (is_anonymous_subtype(elem) && !type_const_bounds(elem))
      elem = type_base_recur(elem);

   return &(td->composite.typeDecl);
}

static c_subTypeDecl *build_subTypeDecl(tree_t decl, c_abstractRegion *region)
{
   type_t type = tree_type(decl);
   assert(!is_anonymous_subtype(type));

   c_subTypeDecl *std = new_object(sizeof(c_subTypeDecl), vhpiSubtypeDeclK);
   init_typeDecl(&std->typeDecl, decl, type, region);

   std->Size = std->typeDecl.IsScalar ? 1 : vhpiUndefined;
   std->Constraints.fn = vhpi_lazy_constraints;

   c_vhpiObject *tobj = &(std->typeDecl.decl.object);
   vhpi_list_add(&region->Decls.list, tobj);

   return std;
}

static c_typeDecl *build_typeDecl(tree_t decl, c_abstractRegion *region)
{
   type_t type = tree_type(decl);

   switch (type_kind(type)) {
   case T_INTEGER:
      {
         c_intTypeDecl *td =
            new_object(sizeof(c_intTypeDecl), vhpiIntTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type, region);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&(region->Decls.list), tobj);

         td->constraint = &(build_int_range(range_of(type, 0))->range);

         return &(td->scalar.typeDecl);
      }
   case T_ENUM:
      {
         c_enumTypeDecl *td =
            new_object(sizeof(c_enumTypeDecl), vhpiEnumTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type, region);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&region->Decls.list, tobj);

         td->EnumLiterals.fn = vhpi_lazy_enum_literals;

         return &(td->scalar.typeDecl);
      }
   case T_PHYSICAL:
      {
         c_physTypeDecl *td =
            new_object(sizeof(c_physTypeDecl), vhpiPhysTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type, region);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&region->Decls.list, tobj);

         td->UnitDecls.fn = vhpi_lazy_unit_decls;

         td->constraint = &(build_phys_range(range_of(type, 0))->range);

         return &(td->scalar.typeDecl);
      }
   case T_REAL:
      {
         c_floatTypeDecl *td =
            new_object(sizeof(c_floatTypeDecl), vhpiFloatTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type, region);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&region->Decls.list, tobj);

         td->constraint = &(build_float_range(range_of(type, 0))->range);

         return &(td->scalar.typeDecl);
      }
   case T_ARRAY:
      return build_arrayTypeDecl(type, decl, region);
   case T_SUBTYPE:
      {
         type_t base = type_base(type);
         assert(type_kind(base) == T_ARRAY);

         c_arrayTypeDecl *atd =
            new_object(sizeof(c_arrayTypeDecl), vhpiArrayTypeDeclK);
         init_compositeTypeDecl(&(atd->composite), decl, base, region);

         atd->NumDimensions = dimension_of(type);

         c_subTypeDecl *std = build_subTypeDecl(decl, region);

         std->BaseType = &(atd->composite.typeDecl);

         return &(std->typeDecl);
      }
   case T_RECORD:
      {
         c_recordTypeDecl *td =
            new_object(sizeof(c_recordTypeDecl), vhpiRecordTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl, type, region);

         c_vhpiObject *tobj = &(td->composite.typeDecl.decl.object);
         vhpi_list_add(&region->Decls.list, tobj);

         td->RecordElems.fn = vhpi_lazy_fields;

         return &(td->composite.typeDecl);
      }
   case T_INCOMPLETE:
      return NULL;   // Ignore it
   case T_ACCESS:
      {
         c_accessTypeDecl *td =
            new_object(sizeof(c_accessTypeDecl), vhpiAccessTypeDeclK);
         init_typeDecl(&td->typeDecl, decl, type, region);

         vhpi_list_add(&region->Decls.list, &(td->typeDecl.decl.object));
         hash_put(vhpi_context()->objcache, type, td);

         return &(td->typeDecl);
      }
   case T_FILE:
      {
         c_fileTypeDecl *td =
            new_object(sizeof(c_fileTypeDecl), vhpiFileTypeDeclK);
         init_typeDecl(&td->typeDecl, decl, type, region);

         vhpi_list_add(&region->Decls.list, &(td->typeDecl.decl.object));
         hash_put(vhpi_context()->objcache, type, td);

         return &(td->typeDecl);
      }
   default:
      fatal_trace("cannot build VHPI typeDecl for %pK %pT", type, type);
   }
}

static c_designUnit *build_designUnit(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ENTITY:
      {
         c_entityDecl *entity =
            new_object(sizeof(c_entityDecl), vhpiEntityDeclK);
         init_entityDecl(entity, t);

         return &(entity->designUnit);
      }
   case T_ARCH:
      {
         c_designUnit *primary = cached_designUnit(tree_primary(t));

         c_secondaryUnit *secondary =
            new_object(sizeof(c_secondaryUnit), vhpiArchBodyK);
         init_secondaryUnit(secondary, t, primary);

         return &(secondary->designUnit);
      }
   case T_PACKAGE:
      {
         c_packDecl *pack = new_object(sizeof(c_packDecl), vhpiPackDeclK);
         init_packDecl(pack, t);

         return &(pack->designUnit);
      }
   case T_VERILOG:
      {
         c_verilogModule *mod =
            new_object(sizeof(c_verilogModule), vhpiVerilogModuleK);
         init_verilogModule(mod, t);

         return &(mod->designUnit);
      }
   default:
      fatal_trace("unsupported tree kind %pK in build_designUnit", t);
   }
}

static c_designUnit *cached_designUnit(tree_t t)
{
   hash_t *cache = vhpi_context()->objcache;
   c_designUnit *du = hash_get(cache, t);
   if (du == NULL) {
      du = build_designUnit(t);
      hash_put(cache, t, du);
   }

   return du;
}

static void build_genericDecl(tree_t generic, int pos,
                              c_abstractRegion *region)
{
   c_genericDecl *g = new_object(sizeof(c_genericDecl), vhpiGenericDeclK);
   init_interfaceDecl(&(g->interface), generic, pos, region);

   g->IsLocal = (tree_kind(region->object.tree) == T_COMPONENT);
   g->IsVital = false;
   g->Mode = mode_map[tree_subkind(generic)];

   vhpi_list_add(&region->Decls.list, &(g->interface.objDecl.decl.object));
}

static c_paramDecl *build_constParamDecl(tree_t param, int pos, int argslot,
                                         c_abstractRegion *region)
{
   c_constParamDecl *p = new_object(sizeof(c_constParamDecl),
                                    vhpiConstParamDeclK);
   init_paramDecl(&(p->param), param, pos, argslot, region);

   p->Mode = mode_map[tree_subkind(param)];
   return (&p->param);
}

static c_paramDecl *build_varParamDecl(tree_t param, int pos, int argslot,
                                         c_abstractRegion *region)
{
   c_varParamDecl *p = new_object(sizeof(c_varParamDecl), vhpiVarParamDeclK);
   init_paramDecl(&(p->param), param, pos, argslot, region);

   p->Mode = mode_map[tree_subkind(param)];
   return &(p->param);
}

static void build_portDecl(tree_t port, int pos,
                           c_abstractRegion *region)
{
   c_portDecl *p = new_object(sizeof(c_portDecl), vhpiPortDeclK);
   init_interfaceDecl(&(p->interface), port, pos, region);

   p->Mode = mode_map[tree_subkind(port)];

   vhpi_list_add(&region->Decls.list, &(p->interface.objDecl.decl.object));
}

static void build_signalDecl(tree_t decl, c_abstractRegion *region)
{
   c_sigDecl *s = new_object(sizeof(c_sigDecl), vhpiSigDeclK);
   init_objDecl(&(s->objDecl), decl, region);

   vhpi_list_add(&region->Decls.list, &(s->objDecl.decl.object));
}

static c_constDecl *build_constDecl(tree_t decl, c_abstractRegion *region)
{
   c_constDecl *cd = new_object(sizeof(c_constDecl), vhpiConstDeclK);
   init_objDecl(&(cd->objDecl), decl, region);

   cd->IsDeferred = !tree_has_value(decl);

   vhpi_list_add(&region->Decls.list, &(cd->objDecl.decl.object));
   return cd;
}

static void build_paramDecls(tree_t decl, int first, c_subpDecl *subp)
{
   const int nparams = tree_ports(decl);
   vhpi_list_reserve(&subp->Params, nparams);

   for (int i = 0, slot = first; i < nparams; i++) {
      tree_t p = tree_port(decl, i);
      c_paramDecl *pd;
      switch (tree_class(p)) {
      case C_CONSTANT:
         pd = build_constParamDecl(p, i, slot, subp->decl.ImmRegion);
         break;
      case C_VARIABLE:
         pd = build_varParamDecl(p, i, slot, subp->decl.ImmRegion);
         break;
      default:
         fatal_at(tree_loc(p), "unsupported parameter class");
      }

      vhpi_list_add(&subp->Params, &(pd->interface.objDecl.decl.object));

      type_t type = tree_type(p);
      if (type_is_array(type) && !type_const_bounds(type)) {
         pd->wrapped = true;
         slot += 1 + 2*dimension_of(type);
      }
      else
         slot += 1;
   }
}

static c_funcDecl *build_funcDecl(tree_t decl, c_abstractRegion *region)
{
   c_funcDecl *f = new_object(sizeof(c_funcDecl), vhpiFuncDeclK);
   init_subpDecl(&(f->subpDecl), decl, region);

   build_paramDecls(decl, 1, &(f->subpDecl));

   f->IsPure = !(tree_flags(decl) & TREE_F_IMPURE);
   return f;
}

static c_procDecl *build_procDecl(tree_t decl, c_abstractRegion *region)
{
   c_procDecl *f = new_object(sizeof(c_funcDecl), vhpiProcDeclK);
   init_subpDecl(&(f->subpDecl), decl, region);

   build_paramDecls(decl, 2, &(f->subpDecl));

   return f;
}

static c_abstractRegion *build_blockStmt(tree_t t, c_abstractRegion *region)
{
   c_blockStmt *bs = new_object(sizeof(c_blockStmt), vhpiBlockStmtK);
   init_abstractRegion(&(bs->region), region, t);
   init_stmt(&(bs->stmt), t);

   if (tree_decls(t) > 1) {
      tree_t d1 = tree_decl(t, 1);
      bs->IsGuarded = (tree_kind(d1) == T_IMPLICIT_SIGNAL
                       && tree_subkind(d1) == IMPLICIT_GUARD);
   }

   vhpi_list_add(&region->Stmts.list, &(bs->region.object));

   return &(bs->region);
}

static c_abstractRegion *build_compInstStmt(tree_t t, tree_t inst,
                                            c_abstractRegion *region)
{
   assert(tree_kind(t) == T_BLOCK);

   tree_t inner = t;
   c_designUnit *du = NULL;
   switch (tree_kind(inst)) {
   case T_ARCH:
      du = cached_designUnit(inst);
      break;
   case T_COMPONENT:
      if (tree_stmts(t) >= 1) {
         inner = tree_stmt(t, 0);
         assert(tree_kind(inner) == T_BLOCK);

         tree_t h = tree_decl(inner, 0);
         assert(tree_kind(h) == T_HIER);

         du = cached_designUnit(tree_ref(h));
      }
      break;
   default:
      fatal_trace("unexpected instance kind %s in build_compInstStmt",
                  tree_kind_str(tree_kind(inst)));
   }

   c_compInstStmt *c = new_object(sizeof(c_compInstStmt), vhpiCompInstStmtK);
   init_designInstUnit(&(c->designInstUnit), region, t, du);
   init_stmt(&(c->stmt), t);

   // Make sure all lookups happen in the implicit inner region
   (void)vhpi_get_name(&c->designInstUnit.region.object);
   (void)vhpi_get_full_name(&c->designInstUnit.region.object);
   c->designInstUnit.region.object.tree = inner;

   vhpi_list_add(&region->Stmts.list, &(c->designInstUnit.region.object));

   return &(c->designInstUnit.region);
}

static c_abstractRegion *build_forGenerate(tree_t t, c_abstractRegion *region)
{
   c_forGenerate *g = new_object(sizeof(c_forGenerate), vhpiForGenerateK);
   init_abstractRegion(&(g->region), region, t);
   init_stmt(&(g->stmt), t);

   assert(tree_generics(t) == 1);
   assert(tree_genmaps(t) == 1);

   tree_t index = tree_value(tree_genmap(t, 0));
   g->GenerateIndex = assume_int(index);

   vhpi_list_reserve(&(g->region.Decls.list), tree_decls(t) + 1);

   tree_t g0 = tree_generic(t, 0);
   g->ParamDecl = build_constDecl(g0, &(g->region));

   vhpi_list_add(&region->Stmts.list, &(g->region.object));

   return &(g->region);
}

static c_abstractRegion *build_ifGenerate(tree_t t, c_abstractRegion *region)
{
   c_ifGenerate *g = new_object(sizeof(c_forGenerate), vhpiIfGenerateK);
   init_abstractRegion(&(g->region), region, t);
   init_stmt(&(g->stmt), t);

   vhpi_list_add(&region->Stmts.list, &(g->region.object));

   return &(g->region);
}

static void build_packInst(tree_t inst, c_abstractRegion *region)
{
   c_designUnit *du = cached_designUnit(tree_ref(inst));

   c_packInst *pi = new_object(sizeof(c_packInst), vhpiPackInstK);
   init_designInstUnit(&(pi->designInstUnit), region, inst, du);
}

static c_tool *build_tool(int argc, char **argv)
{
   c_tool *tool = new_object(sizeof(c_tool), vhpiToolK);
   tool->capabilities = vhpiProvidesPostAnalysis
      | vhpiProvidesStaticAccess | vhpiProvidesHierarchy
      | vhpiProvidesConnectivity | vhpiProvidesDynamicElab;

   vhpi_list_reserve(&tool->argv, argc);

   for (int i = 0; i < argc; i++) {
      c_argv *arg = new_object(sizeof(c_argv), vhpiArgvK);
      arg->StrVal = new_string(argv[i]);
      vhpi_list_add(&tool->argv, &(arg->object));
   }

   return tool;
}

static void vhpi_lazy_selected_names(c_vhpiObject *obj)
{
   vhpiObjectListT *list = NULL;

   c_name *n = is_name(obj);
   if (n != NULL)
      list = &(n->SelectedNames.list);

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL)
      list = &(od->SelectedNames.list);

   assert(list != NULL);
   assert(list->count == 0);

   c_typeDecl *td = vhpi_get_type(obj);
   assert(td != NULL);

   c_subTypeDecl *std = is_subTypeDecl(&(td->decl.object));
   if (std != NULL) {
      vhpiObjectListT *cons =
         expand_lazy_list(&(td->decl.object), &(std->Constraints));

      vhpi_list_reserve(list, cons->count);

      for (int i = 0; i < cons->count; i++) {
         c_elemDecl *ed = is_elemDecl(cons->items[i]);
         if (ed == NULL)
            continue;

         c_selectedName *sn =
            new_object(sizeof(c_selectedName), vhpiSelectedNameK);
         init_selectedName(sn, obj, ed);

         vhpi_list_add(list, &(sn->prefixedName.name.expr.object));
      }
   }

   c_recordTypeDecl *rtd = is_recordTypeDecl(&(td->decl.object));
   if (rtd != NULL) {
      vhpiObjectListT *elems =
         expand_lazy_list(&(td->decl.object), &(rtd->RecordElems));

      vhpi_list_reserve(list, elems->count);

      for (int i = 0; i < elems->count; i++) {
         c_elemDecl *ed = is_elemDecl(elems->items[i]);
         assert(ed != NULL);

         c_selectedName *sn =
            new_object(sizeof(c_selectedName), vhpiSelectedNameK);
         init_selectedName(sn, obj, ed);

         vhpi_list_add(list, &(sn->prefixedName.name.expr.object));
      }
   }
}

static void vhpi_lazy_indexed_names(c_vhpiObject *obj)
{
   vhpiObjectListT *list = NULL;

   c_name *n = is_name(obj);
   if (n != NULL)
      list = &(n->IndexedNames.list);

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL)
      list = &(od->IndexedNames.list);

   assert(list != NULL);
   assert(list->count == 0);

   c_typeDecl *td = vhpi_get_type(obj);
   assert(td != NULL);

   c_typeDecl *et = vhpi_get_elem_type(&(td->decl.object));
   if (et == NULL)
      return;

   vhpiObjectListT local_constraints = {};
   vhpiObjectListT *Constraints;
   c_subTypeDecl *std = is_subTypeDecl(&(td->decl.object));
   if (std != NULL)
      Constraints = expand_lazy_list(&std->typeDecl.decl.object,
                                     &(std->Constraints));
   else if (td->IsUnconstrained && type_is_array(td->type)) {
      // TODO: temporary workaround for unconstrained arrays
      const vhpiIntT size = vhpi_get_size(obj);
      if (size == vhpiUndefined || size == 0)
         return;

      c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
      init_range(&(ir->range), true, false, true);
      ir->LeftBound  = 0;
      ir->RightBound = size - 1;

      vhpi_list_reserve(&local_constraints, 1);
      vhpi_list_add(&local_constraints, &(ir->range.object));
      Constraints = &local_constraints;
   }
   else
      return;

   if (Constraints->count == 0)
      return;

   size_t count = 1;
   vhpiIntT *lens LOCAL = xmalloc_array(Constraints->count, sizeof(vhpiIntT));
   for (int i = 0; i < Constraints->count; i++) {
      c_intRange *ir = is_intRange(Constraints->items[i]);
      assert(ir != NULL);

      if ((lens[i] = range_len(ir)) == 0)
         return;

      count *= lens[i];
   }

   vhpi_list_reserve(list, count);

   int pos = 0;
   vhpiIntT *indices LOCAL =
      xcalloc_array(Constraints->count, sizeof(vhpiIntT));
   do {
      c_indexedName *in = new_object(sizeof(c_indexedName), vhpiIndexedNameK);
      init_indexedName(in, et, obj, Constraints, indices);

      vhpi_list_add(list, &(in->prefixedName.name.expr.object));

      for (pos = Constraints->count - 1; pos >= 0; pos--) {
         indices[pos]++;
         if (indices[pos] >= lens[pos])
            indices[pos] = 0;
         else
            break;
      }
   } while (pos > 0 || indices[0] != 0);
}

static void vhpi_lazy_enum_literals(c_vhpiObject *obj)
{
   c_enumTypeDecl *td = is_enumTypeDecl(obj);
   assert(td != NULL);

   const int nlits = type_enum_literals(td->scalar.typeDecl.type);
   vhpi_list_reserve(&td->EnumLiterals.list, nlits);

   for (int i = 0; i < nlits; i++) {
      tree_t lit = type_enum_literal(td->scalar.typeDecl.type, i);
      c_enumLiteral *el = new_object(sizeof(c_enumLiteral), vhpiEnumLiteralK);
      init_enumLiteral(el, lit, td);
      vhpi_list_add(&td->EnumLiterals.list, &(el->decl.object));
   }
}

static void vhpi_lazy_unit_decls(c_vhpiObject *obj)
{
   c_physTypeDecl *td = is_physTypeDecl(obj);
   assert(td != NULL);

   const int units = type_units(td->scalar.typeDecl.type);
   vhpi_list_reserve(&td->UnitDecls.list, units);

   for (int i = 0; i < units; i++) {
      tree_t u = type_unit(td->scalar.typeDecl.type, i);
      c_unitDecl *ud = new_object(sizeof(c_unitDecl), vhpiUnitDeclK);
      init_unitDecl(ud, u, td);
      vhpi_list_add(&td->UnitDecls.list, &(ud->decl.object));
   }
}

static void vhpi_lazy_fields(c_vhpiObject *obj)
{
   c_recordTypeDecl *td = is_recordTypeDecl(obj);
   assert(td != NULL);

   const int nfields = type_fields(td->composite.typeDecl.type);
   vhpi_list_reserve(&td->RecordElems.list, nfields);

   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(td->composite.typeDecl.type, i);

      c_elemDecl *ed = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
      init_elemDecl(ed, f, &(td->composite.typeDecl.decl));

      vhpi_list_add(&td->RecordElems.list, &(ed->decl.object));
   }
}

static void vhpi_lazy_stmts(c_vhpiObject *obj)
{
   c_abstractRegion *r = is_abstractRegion(obj);
   assert(r != NULL);

   if (tree_kind(r->object.tree) == T_PACKAGE)
      return;

   const int nstmts = tree_stmts(r->object.tree);
   vhpi_list_reserve(&r->Stmts.list, nstmts);

   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(r->object.tree, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         {
            tree_t h = tree_decl(s, 0);
            assert(tree_kind(h) == T_HIER);

            switch (tree_subkind(h)) {
            case T_BLOCK:
               build_blockStmt(s, r);
               break;
            case T_ARCH:
            case T_COMPONENT:
               build_compInstStmt(s, tree_ref(h), r);
               break;
            case T_FOR_GENERATE:
               build_forGenerate(s, r);
               break;
            case T_CASE_GENERATE:  // No VHPI kind for this
            case T_IF_GENERATE:
               build_ifGenerate(s, r);
               break;
            default:
               continue;
            }
         }
         break;
      default:
         break;
      }
   }
}

static void vhpi_lazy_decls(c_vhpiObject *obj)
{
   c_abstractRegion *r = is_abstractRegion(obj);
   assert(r != NULL);

   int ndecls = 0, nports = 0, ngenerics = 0;

   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiCompInstStmtK:
   case vhpiEntityDeclK:
   case vhpiBlockStmtK:
      nports = tree_ports(r->object.tree);
      // Fall-through
   case vhpiPackDeclK:
   case vhpiPackInstK:
      ngenerics = tree_generics(r->object.tree);
      ndecls = tree_decls(r->object.tree);
      break;
   case vhpiForGenerateK:
      ndecls = tree_decls(r->object.tree);
      break;
   default:
      break;
   }

   vhpi_list_reserve(&r->Decls.list, ndecls + nports + ngenerics);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(r->object.tree, i);
      build_genericDecl(g, i, r);
   }

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(r->object.tree, i);
      build_portDecl(p, i, r);
   }

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(r->object.tree, i);
      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
         build_signalDecl(d, r);
         break;
      case T_CONST_DECL:
         build_constDecl(d, r);
         break;
      case T_TYPE_DECL:
         build_typeDecl(d, r);
         break;
      case T_SUBTYPE_DECL:
         build_subTypeDecl(d, r);
         break;
      case T_PACK_INST:
         build_packInst(d, r);
         break;
      default:
         break;
      }
   }
}

static void vhpi_lazy_constraints(c_vhpiObject *obj)
{
   c_subTypeDecl *std = is_subTypeDecl(obj);
   assert(std != NULL);

   c_typeDecl *base = vhpi_get_base_type(obj);
   assert(base != NULL);

   c_arrayTypeDecl *atd = is_arrayTypeDecl(&(base->decl.object));
   if (atd != NULL) {
      const int ndims = dimension_of(std->typeDecl.type);
      vhpi_list_reserve(&(std->Constraints.list), ndims);

      for (int i = 0; i < ndims; i++) {
         bool dynamic_bounds = true;
         if (type_kind(std->typeDecl.type) == T_SUBTYPE) {
            tree_t r = range_of(std->typeDecl.type, i);

            int64_t low, high;
            if (folded_bounds(r, &low, &high)) {
               range_kind_t dir = tree_subkind(r);
               int64_t left = (dir == RANGE_TO) ? low : high;
               int64_t right = (dir == RANGE_TO) ? high : low;

               const bool null = dir == RANGE_TO ? left > right : right > left;

               c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
               init_range(&(ir->range), dir == RANGE_TO, null, true);

               ir->LeftBound  = left;
               ir->RightBound = right;

               vhpi_list_add(&std->Constraints.list, &(ir->range.object));

               dynamic_bounds = false;
            }
         }

         if (dynamic_bounds) {
            c_intRange *ir = build_unconstrained(obj, std->depth + i, true);
            vhpi_list_add(&std->Constraints.list, &(ir->range.object));
         }
      }
   }

   c_recordTypeDecl *rtd = is_recordTypeDecl(&(base->decl.object));
   if (rtd != NULL) {
      vhpiObjectListT *elems =
         expand_lazy_list(&(base->decl.object), &rtd->RecordElems);

      vhpi_list_reserve(&(std->Constraints.list), elems->count);

      if (rtd->composite.typeDecl.IsUnconstrained) {
         for (int i = 0; i < elems->count; i++) {
            c_elemDecl *ed = is_elemDecl(elems->items[i]);
            assert(ed != NULL);

            c_typeDecl *et = vhpi_get_type(elems->items[i]);
            assert(et != NULL);

            if (et->IsUnconstrained) {
               c_selectedName *sn =
                  new_object(sizeof(c_selectedName), vhpiSelectedNameK);
               init_selectedName(sn, std->source ?: obj, ed);

               c_vhpiObject *sn_obj = &(sn->prefixedName.name.expr.object);
               c_abstractRegion *r = ed->decl.ImmRegion;

               tree_t cons = type_constraint_for_field(std->typeDecl.type,
                                                       ed->decl.object.tree);
               type_t sub = cons ? tree_type(cons) : et->type;

               c_typeDecl *est = new_anonymous_subtypeDecl(sub, sn_obj, r, 0);

               c_elemDecl *new = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
               init_elemDecl(new, ed->decl.object.tree, &(std->typeDecl.decl));
               new->Type = est;

               vhpi_list_add(&(std->Constraints.list), &(new->decl.object));
            }
            else
               vhpi_list_add(&(std->Constraints.list), &(ed->decl.object));
         }
      }
      else {
         for (int i = 0; i < elems->count; i++)
            vhpi_list_add(&(std->Constraints.list), elems->items[i]);
      }
   }

   c_floatTypeDecl *ftd = is_floatTypeDecl(&(base->decl.object));
   if (ftd != NULL && type_has_constraint(std->typeDecl.type)) {
      tree_t c = type_constraint(std->typeDecl.type);
      assert(tree_subkind(c) == C_RANGE);

      c_floatRange *fr = build_float_range(tree_range(c, 0));

      vhpi_list_reserve(&(std->Constraints.list), 1);
      vhpi_list_add(&(std->Constraints.list), &(fr->range.object));
   }

   c_intTypeDecl *itd = is_intTypeDecl(&(base->decl.object));
   if (itd != NULL && type_has_constraint(std->typeDecl.type)) {
      tree_t c = type_constraint(std->typeDecl.type);
      assert(tree_subkind(c) == C_RANGE);

      c_intRange *ir = build_int_range(tree_range(c, 0));

      vhpi_list_reserve(&(std->Constraints.list), 1);
      vhpi_list_add(&(std->Constraints.list), &(ir->range.object));
   }

   c_physTypeDecl *ptd = is_physTypeDecl(&(base->decl.object));
   if (ptd != NULL && type_has_constraint(std->typeDecl.type)) {
      tree_t c = type_constraint(std->typeDecl.type);
      assert(tree_subkind(c) == C_RANGE);

      c_physRange *pr = build_phys_range(tree_range(c, 0));

      vhpi_list_reserve(&(std->Constraints.list), 1);
      vhpi_list_add(&(std->Constraints.list), &(pr->range.object));
   }
}

static void vhpi_build_deps_cb(tree_t unit, void *ctx)
{
   hset_t *visited = ctx;

   if (hset_contains(visited, unit))
      return;

   hset_insert(visited, unit);

   if (tree_kind(unit) == T_PACKAGE) {
      c_designUnit *pack = cached_designUnit(unit);
      assert(pack->region.object.kind == vhpiPackDeclK);

      c_packInst *pi = new_object(sizeof(c_packInst), vhpiPackInstK);
      init_designInstUnit(&(pi->designInstUnit), NULL, unit, pack);

      vhpi_context_t *c = vhpi_context();
      APUSH(c->packages, &(pi->designInstUnit.region.object));
   }

   tree_walk_deps(unit, vhpi_build_deps_cb, visited);
}

void vhpi_run_callbacks(int32_t reason)
{
   vhpi_context_t *c = vhpi_context();

   int32_t rep = 0;
   switch (reason) {
   case vhpiCbEndOfProcesses:      rep = vhpiCbRepEndOfProcesses; break;
   case vhpiCbLastKnownDeltaCycle: rep = vhpiCbRepLastKnownDeltaCycle; break;
   case vhpiCbNextTimeStep:        rep = vhpiCbRepNextTimeStep; break;
   case vhpiCbEndOfTimeStep:       rep = vhpiCbRepEndOfTimeStep; break;
   case vhpiCbStartOfNextCycle:    rep = vhpiCbRepStartOfNextCycle; break;
   }

   switch (reason) {
   case vhpiCbStartOfAnalysis:    c->phase = vhpiAnalysisPhase; break;
   case vhpiCbStartOfElaboration: c->phase = vhpiElaborationPhase; break;
   case vhpiCbEndOfInitialization:
   case vhpiCbStartOfSimulation:  c->phase = vhpiSimulationPhase; break;
   case vhpiCbEndOfTool:          c->phase = vhpiTerminationPhase; break;
   }

   const int orig_count = c->callbacks.count;
   int wptr = 0;
   for (int i = 0; i < orig_count; i++) {
      vhpiHandleT handle = c->callbacks.items[i];

      handle_slot_t *slot = decode_handle(c, handle);
      if (slot == NULL)
         continue;

      assert(slot->kind == HANDLE_INTERNAL);

      c_callback *cb = is_callback(slot->obj);
      assert(cb != NULL);

      if (cb->Reason == reason)
         vhpi_global_cb(c->model, handle);
      else if (cb->Reason == rep) {
         vhpi_global_cb(c->model, handle);
         c->callbacks.items[wptr++] = handle;
      }
      else
         c->callbacks.items[wptr++] = handle;
   }

   // Callbacks may register additional callbacks
   for (int i = orig_count; i < c->callbacks.count; i++)
      c->callbacks.items[wptr++] = c->callbacks.items[i];

   ATRIM(c->callbacks, wptr);
}

static void vhpi_initialise_cb(rt_model_t *m, void *arg)
{
   vhpi_context_t *c = arg;
   vhpi_clear_error();

   assert(tree_kind(c->top) == T_ELAB);

   tree_t b0 = tree_stmt(c->top, 0);
   if (tree_kind(b0) == T_VERILOG)
      fatal_at(tree_loc(b0), "Verilog top-level modules are not supported "
               "by VHPI");

   assert(tree_kind(b0) == T_BLOCK);

   tree_t h = tree_decl(b0, 0);
   assert(tree_kind(h) == T_HIER);

   tree_t s = tree_ref(h);
   assert(tree_kind(s) == T_ARCH);

   c_designUnit *du = cached_designUnit(s);

   c->root = new_object(sizeof(c_rootInst), vhpiRootInstK);
   init_designInstUnit(&(c->root->designInstUnit), NULL, b0, du);

   vhpi_run_callbacks(vhpiCbEndOfInitialization);
}

static void vhpi_phase_cb(rt_model_t *m, void *arg)
{
   const int32_t reason = (intptr_t)arg;

   vhpi_run_callbacks(reason);

   switch (reason) {
   case vhpiCbEndOfProcesses:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbNextTimeStep:
   case vhpiCbEndOfTimeStep:
   case vhpiCbStartOfNextCycle:
      model_set_phase_cb(m, vhpi_get_phase(reason), vhpi_phase_cb,
                          (void *)(intptr_t)reason);
      break;
   }
}

vhpi_context_t *vhpi_context_new(void)
{
   assert(global_context == NULL);

   vhpi_context_t *c = global_context = xcalloc(sizeof(vhpi_context_t));
   c->objcache = hash_new(128);
   c->pool     = pool_new();
   c->phase    = vhpiRegistrationPhase;

   return c;
}

void vhpi_context_initialise(vhpi_context_t *c, tree_t top, rt_model_t *model,
                             jit_t *jit)
{
   assert(c->model == NULL);
   assert(c->top == NULL);
   assert(c->jit == NULL);

   c->model = model;
   c->top   = top;
   c->jit   = jit;

   hset_t *visited = hset_new(64);
   tree_walk_deps(c->top, vhpi_build_deps_cb, visited);
   hset_free(visited);

   model_set_phase_cb(model, END_OF_INITIALISATION, vhpi_initialise_cb, c);

   static const int32_t reasons[] = {
      vhpiCbStartOfSimulation,
      vhpiCbEndOfSimulation,
      vhpiCbNextTimeStep,
      vhpiCbEndOfTimeStep,
      vhpiCbStartOfNextCycle,
      vhpiCbLastKnownDeltaCycle,
      vhpiCbEndOfProcesses
   };

   for (size_t i = 0; i < ARRAY_LEN(reasons); i++)
      model_set_phase_cb(model, vhpi_get_phase(reasons[i]),
                          vhpi_phase_cb, (void *)(uintptr_t)reasons[i]);
}

void vhpi_set_plusargs(vhpi_context_t *c, int argc, char **argv)
{
   if (c->tool == NULL)
      c->tool = build_tool(argc, argv);
   else {
      // TODO: deprecate plusargs after run option and make this an
      // assertion failure
      vhpi_list_reserve(&c->tool->argv, argc);

      for (int i = 0; i < argc; i++) {
         c_argv *arg = new_object(sizeof(c_argv), vhpiArgvK);
         arg->StrVal = new_string(argv[i]);
         vhpi_list_add(&c->tool->argv, &(arg->object));
      }
   }
}

static void vhpi_handles_diag(vhpi_context_t *c, diag_t *d, handle_kind_t kind)
{
   for (int i = 0; i < c->num_handles; i++) {
      handle_slot_t *slot = &(c->handles[i]);
      if (slot->obj == NULL || slot->kind != kind)
         continue;

      diag_printf(d, "\n%s", handle_pp(encode_handle(slot, i)));

      c_refcounted *rc = is_refcounted(slot->obj);
      if (rc != NULL)
         diag_printf(d, " with %d reference%s", rc->refcount,
                     rc->refcount != 1 ? "s" : "");
   }
}

static void vhpi_check_leaks(vhpi_context_t *c)
{
   int nuser = 0, ninternal UNUSED = 0;
   for (int i = 0; i < c->num_handles; i++) {
      if (c->handles[i].obj == NULL)
         continue;
      else if (c->handles[i].kind == HANDLE_INTERNAL)
         ninternal++;
      else
         nuser++;
   }

   if (nuser > 0) {
      diag_t *d = diag_new(DIAG_WARN, NULL);
      diag_printf(d, "VHPI program exited with %d active handles", nuser);
      vhpi_handles_diag(c, d, HANDLE_USER);
      diag_emit(d);
   }

#ifdef DEBUG
   if (ninternal > 0) {
      diag_t *d = diag_new(DIAG_DEBUG, NULL);
      diag_printf(d, "VHPI program exited with %d active internal handles",
                  ninternal);
      vhpi_handles_diag(c, d, HANDLE_INTERNAL);
      diag_emit(d);
   }
#endif
}

void vhpi_context_free(vhpi_context_t *c)
{
   vhpi_run_callbacks(vhpiCbEndOfTool);

   for (int i = 0; i < c->foreignfs.count; i++) {
      c_foreignf *f = is_foreignf(c->foreignfs.items[i]);
      assert(f != NULL);
      drop_handle(c, f->handle);
   }
   ACLEAR(c->foreignfs);

   for (int i = 0; i < c->callbacks.count; i++)
      drop_handle(c, c->callbacks.items[i]);
   ACLEAR(c->callbacks);

   ACLEAR(c->packages);
   ACLEAR(c->recycle);

   if (opt_get_int(OPT_PLI_DEBUG))
      vhpi_check_leaks(c);

   assert(c == global_context);
   global_context = NULL;

   if (c->strtab != NULL)
      shash_free(c->strtab);

#ifdef DEBUG
   size_t alloc, npages;
   pool_stats(c->pool, &alloc, &npages);
   if (npages > 0)
      debugf("VHPI allocated %zu bytes in %zu pages", alloc, npages);
#endif

   hash_free(c->objcache);
   pool_free(c->pool);
   free(c->handles);
   free(c);
}

////////////////////////////////////////////////////////////////////////////////
// Foreign function interface

vhpiHandleT vhpi_bind_foreign(const char *obj_lib, const char *model,
                              tree_t where, tree_t region)
{
   c_abstractRegion *r;
   switch (tree_kind(region)) {
   case T_PACKAGE:
      r = &(cached_designUnit(region)->region);
      break;
   case T_BLOCK:
      {
         vhpi_context_t *c = vhpi_context();
         if (c->model == NULL) {
            // Hack for when called during elaboration
            tree_t h = tree_decl(region, 0);
            assert(tree_kind(h) == T_HIER);

            r = &(cached_designUnit(tree_ref(h))->region);
         }
         else {
            rt_scope_t *s = find_scope(c->model, region);
            if (s == NULL)
               jit_msg(NULL, DIAG_FATAL, "cannot find scope for %pI",
                    tree_ident(region));

            if ((r = vhpi_get_region(s)) == NULL)
               jit_msg(NULL, DIAG_FATAL, "cannot find region for %pI",
                       tree_ident(region));
         }
      }
      break;
   default:
      jit_msg(NULL, DIAG_FATAL, "unsupported region %pI for foreign subprogram",
              tree_ident(region));
   }

   vhpi_context_t *c = vhpi_context();
   for (int i = 0; i < c->foreignfs.count; i++) {
      c_foreignf *f = cast_foreignf(c->foreignfs.items[i]);
      if (strcmp(f->data.libraryName, obj_lib))
         continue;
      else if (strcmp(f->data.modelName, model))
         continue;

      assert(tree_kind(where) == T_ATTR_SPEC);
      tree_t sub = tree_ref(where);

      if (f->decl != NULL && f->decl->decl.object.tree == sub)
         return f->handle;
      else if (f->decl != NULL)
         jit_msg(NULL, DIAG_FATAL, "foreign subprogram %s/%s already bound "
                 "to %pT", obj_lib, model,
                 tree_type(f->decl->decl.object.tree));

      switch (tree_kind(sub)) {
      case T_FUNC_DECL:
      case T_FUNC_BODY:
         f->decl = &(build_funcDecl(sub, r)->subpDecl);
         break;
      case T_PROC_DECL:
      case T_PROC_BODY:
         f->decl = &(build_procDecl(sub, r)->subpDecl);
         break;
      default:
         jit_msg(NULL, DIAG_FATAL, "unsupported foreign subprogram");
      }

      return (f->handle = internal_handle_for(&(f->object)));
   }

   return NULL;
}

void vhpi_call_foreign(vhpiHandleT handle, jit_scalar_t *args, tlab_t *tlab)
{
   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      jit_msg(NULL, DIAG_FATAL, "called invalid foreign subprogram");

   c_foreignf *f = is_foreignf(obj);
   assert(f != NULL);

   void *orig_p0 = args[0].pointer;

   vhpi_context_t *c = vhpi_context();
   assert(c->args == NULL);
   c->args = args;
   c->tlab = tlab;

   c_funcDecl *fd = is_funcDecl(&(f->decl->decl.object));
   if (fd != NULL)
      fd->size = vhpiUndefined;

   for (int i = 0; i < f->decl->Params.count; i++) {
      c_paramDecl *pd = is_paramDecl(f->decl->Params.items[i]);
      assert(pd != NULL);

      // Invalidate any cached argument sizes
      if (pd->interface.objDecl.Type != NULL) {
         c_subTypeDecl *std =
            is_subTypeDecl(&(pd->interface.objDecl.Type->decl.object));
         if (std != NULL)
            std->Size = vhpiUndefined;
      }
   }

   // TODO: don't get/drop this every time?
   vhpiHandleT subp = internal_handle_for(&(f->decl->decl.object));
   vhpiCbDataT data = {
      .obj = subp
   };
   (*f->data.execf)(&data);

   drop_handle(c, subp);
   c->args = NULL;
   c->tlab = NULL;

   if (f->decl->decl.object.kind == vhpiFuncDeclK && args[0].pointer == orig_p0)
      jit_msg(NULL, DIAG_FATAL, "foreign function %pT did not return a value",
              tree_type(f->decl->decl.object.tree));
}
