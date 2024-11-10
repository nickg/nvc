//
//  Copyright (C) 2014-2024  Nick Gasson
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
#include "rt/model.h"
#include "rt/rt.h"
#include "rt/structs.h"
#include "tree.h"
#include "type.h"
#include "vhpi/vhpi-macros.h"
#include "vhpi/vhpi-util.h"

#include <assert.h>
#include <math.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef vhpiCharT      *vhpiStringT;
typedef vhpiSmallEnumT  vhpiBooleanT;

typedef struct {
   vhpiClassKindT kind;
   vhpiHandleT    handle;
   loc_t          loc;
} c_vhpiObject;

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
} c_tool;

DEF_CLASS(tool, vhpiToolK, object);

typedef struct tag_abstractRegion c_abstractRegion;
typedef struct tag_expr c_expr;

typedef struct tag_abstractRegion {
   c_vhpiObject      object;
   tree_t            tree;
   rt_scope_t       *scope;
   vhpiLazyListT     decls;
   vhpiLazyListT     stmts;
   c_abstractRegion *UpperRegion;
   vhpiIntT          LineOffset;
   vhpiIntT          LineNo;
   vhpiStringT       FileName;
   vhpiStringT       DefName;
   vhpiStringT       CaseName;
   vhpiStringT       Name;
   vhpiStringT       FullCaseName;
   vhpiStringT       FullName;
   jit_handle_t      handle;
} c_abstractRegion;

typedef struct {
   c_vhpiObject      object;
   type_t            type;
   tree_t            tree;
   c_abstractRegion *ImmRegion;
   vhpiIntT          LineOffset;
   vhpiIntT          LineNo;
   vhpiStringT       FileName;
   vhpiStringT       DefName;
   vhpiStringT       CaseName;
   vhpiStringT       Name;
   vhpiStringT       FullCaseName;
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
   c_range      range;
   vhpiLongIntT LeftBound;
   vhpiLongIntT RightBound;
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
   c_typeDecl     *BaseType;   // XXX: should belong to subtype?
   vhpiFormatT     format;
   const char     *map_str;
   vhpiIntT        numElems;
   vhpiBooleanT    IsAnonymous;   // XXX: should belong to subtype?
   vhpiBooleanT    IsComposite;
   vhpiBooleanT    IsScalar;
   vhpiBooleanT    IsUnconstrained;
   bool            homogeneous;
   bool            wrapped;
   uint8_t         size;
} c_typeDecl;

typedef struct {
   c_typeDecl      typeDecl;
   vhpiObjectListT Constraints;
   vhpiBooleanT    isResolved;
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
} c_physTypeDecl;

DEF_CLASS(physTypeDecl, vhpiPhysTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_scalarTypeDecl scalar;
   c_range         *constraint;
} c_floatTypeDecl;

typedef struct {
   c_compositeTypeDecl composite;
   c_typeDecl         *ElemType;
   vhpiIntT            NumDimensions;
   vhpiObjectListT     Constraints;
} c_arrayTypeDecl;

DEF_CLASS(arrayTypeDecl, vhpiArrayTypeDeclK, composite.typeDecl.decl.object);

typedef struct {
   c_compositeTypeDecl composite;
   vhpiLazyListT       RecordElems;
} c_recordTypeDecl;

DEF_CLASS(recordTypeDecl, vhpiRecordTypeDeclK, composite.typeDecl.decl.object);

typedef struct {
   c_abstractDecl    decl;
   c_typeDecl       *Type;
   c_recordTypeDecl *parent;
   vhpiIntT          Position;
} c_elemDecl;

DEF_CLASS(elemDecl, vhpiElemDeclK, decl.object);

typedef struct {
   c_abstractDecl   decl;
   vhpiLazyListT    IndexedNames;
   vhpiLazyListT    SelectedNames;
   c_typeDecl      *Type;
   vhpiIntT         Access;
   vhpiStaticnessT  Staticness;
   vhpiBooleanT     IsDynamic;
   rt_signal_t     *signal;
   rt_scope_t      *scope;
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
   c_vhpiObject    object;
   tree_t          tree;
   ident_t         module;
   ptrdiff_t       offset;
   vhpiObjectListT Params;
} c_subpDecl;

typedef struct {
   c_subpDecl    subpDecl;
   c_typeDecl   *ReturnType;
   vhpiBooleanT  IsPure;
} c_funcDecl;

DEF_CLASS(funcDecl, vhpiFuncDeclK, subpDecl.object);

typedef struct {
   c_subpDecl subpDecl;
} c_procDecl;

DEF_CLASS(procDecl, vhpiProcDeclK, subpDecl.object);

typedef struct {
   c_objDecl   objDecl;
   vhpiIntT    Position;
   unsigned    argslot;
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
   vhpiModeT       Mode;
} c_constParamDecl;

DEF_CLASS(constParamDecl, vhpiConstParamDeclK, interface.objDecl.decl.object);

typedef struct {
   c_interfaceDecl interface;
   vhpiModeT       Mode;
} c_varParamDecl;

DEF_CLASS(varParamDecl, vhpiVarParamDeclK, interface.objDecl.decl.object);

typedef struct {
   c_objDecl    objDecl;
   vhpiBooleanT IsDeferred;
} c_constDecl;

DEF_CLASS(constDecl, vhpiConstDeclK, objDecl.decl.object);

typedef struct {
   c_abstractDecl  decl;
   vhpiStringT     StrVal;
   vhpiStringT     SignatureName;
   c_enumTypeDecl *Type;
   vhpiIntT        Position;
} c_enumLiteral;

DEF_CLASS(enumLiteral, vhpiEnumLiteralK, decl.object);

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
   vhpiStringT   FullCaseName;
   vhpiStringT   CaseName;
   vhpiStringT   FullName;
   vhpiStringT   Name;
   vhpiStringT   DefName;
   vhpiAccessT   Access;
} c_name;

typedef struct {
   c_name        name;
   c_vhpiObject *Prefix;
   rt_signal_t  *signal;
   rt_scope_t   *scope;
} c_prefixedName;

typedef struct {
   c_prefixedName prefixedName;
   vhpiIntT       BaseIndex;
   int            offset;
} c_indexedName;

DEF_CLASS(indexedName, vhpiIndexedNameK, prefixedName.name.expr.object);

typedef struct {
   c_prefixedName  prefixedName;
   c_elemDecl     *Suffix;
} c_selectedName;

DEF_CLASS(selectedName, vhpiSelectedNameK, prefixedName.name.expr.object);

typedef struct {
   c_abstractRegion region;
   vhpiStringT      CaseName;
   vhpiStringT      Name;
   vhpiStringT      UnitName;
   vhpiObjectListT  DepUnits;
} c_designUnit;

typedef struct {
   c_designUnit    designUnit;
   c_designUnit   *PrimaryUnit;
} c_secondaryUnit;

DEF_CLASS(secondaryUnit, vhpiArchBodyK, designUnit.region.object);

typedef struct {
   c_designUnit    designUnit;
   vhpiObjectListT ports;
} c_entityDecl;

typedef struct {
   c_designUnit designUnit;
} c_packDecl;

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
   c_vhpiObject  object;
   vhpiStateT    State;
   vhpiEnumT     Reason;
   vhpiCbDataT   data;
   rt_watch_t   *watch;
} c_callback;

DEF_CLASS(callback, vhpiCallbackK, object);

typedef void *(*vhpiFilterT)(c_vhpiObject *);

typedef struct {
   c_vhpiObject     object;
   vhpiObjectListT *list;
   c_vhpiObject    *single;
   uint32_t         pos;
   vhpiFilterT      filter;
} c_iterator;

DEF_CLASS(iterator, vhpiIteratorK, object);

typedef struct {
   c_vhpiObject      object;
   vhpiForeignDataT  data;
   c_subpDecl       *decl;
   vhpiHandleT       handle;
} c_foreignf;

DEF_CLASS(foreignf, vhpiForeignfK, object);

#define HANDLE_BITS      (sizeof(vhpiHandleT) * 8)
#define HANDLE_MAX_INDEX ((UINT64_C(1) << (HANDLE_BITS / 2)) - 1)

typedef struct {
   c_vhpiObject *obj;
   uint32_t      refcount;
   uint32_t      generation;
} handle_slot_t;

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
} vhpi_context_t;

static c_typeDecl *cached_typeDecl(type_t type, c_vhpiObject *obj);
static c_designUnit *cached_designUnit(tree_t t);
static c_typeDecl *build_dynamicSubtype(c_typeDecl *base, void *ptr,
                                        vhpiClassKindT kind);
static void *vhpi_get_value_ptr(c_vhpiObject *obj);
static c_typeDecl *vhpi_get_type(c_vhpiObject *obj);
static vhpiClassKindT vhpi_get_prefix_kind(c_vhpiObject *obj);
static void vhpi_lazy_decls(c_vhpiObject *obj);
static void vhpi_lazy_selected_names(c_vhpiObject *obj);
static void vhpi_lazy_indexed_names(c_vhpiObject *obj);
static void vhpi_lazy_enum_literals(c_vhpiObject *obj);
static void vhpi_lazy_fields(c_vhpiObject *obj);
static const char *handle_pp(vhpiHandleT handle);

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

   assert(slot->refcount > 0);
   assert(slot->obj->handle == handle);

   return slot;
}

static vhpiHandleT handle_for(c_vhpiObject *obj)
{
   assert(obj != NULL);

   vhpi_context_t *c = vhpi_context();

   if (obj->handle != NULL) {
      handle_slot_t *slot = decode_handle(c, obj->handle);
      assert(slot != NULL);
      assert(slot->refcount > 0);
      assert(slot->obj == obj);

      slot->refcount++;
      return obj->handle;
   }

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
         c->handles[i].refcount = 0;
         c->handles[i].generation = 1;
      }
   }

   handle_slot_t *slot = &(c->handles[index]);
   assert(slot->refcount == 0);
   slot->refcount = 1;
   slot->obj = obj;

   c->free_hint = index + 1;

   const uintptr_t bits = (uintptr_t)slot->generation << HANDLE_BITS/2 | index;
   return (obj->handle = (vhpiHandleT)bits);
}

static void drop_handle(vhpi_context_t *c, vhpiHandleT handle)
{
   handle_slot_t *slot = decode_handle(c, handle);
   if (slot == NULL)
      return;

   assert(slot->refcount > 0);
   if (--(slot->refcount) > 0)
      return;

   c_vhpiObject *obj = slot->obj;
   slot->obj = NULL;
   slot->generation++;

   obj->handle = NULL;

   c->free_hint = slot - c->handles;

   switch (obj->kind) {
   case vhpiCallbackK:
   case vhpiIteratorK:
      APUSH(c->recycle, obj);
      break;
   default:
      break;
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

static c_abstractRegion *is_abstractRegion(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiBlockStmtK:
   case vhpiCompInstStmtK:
   case vhpiForGenerateK:
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

static c_abstractDecl *cast_abstractDecl(c_vhpiObject *obj)
{
   c_abstractDecl *decl = is_abstractDecl(obj);
   if (decl == NULL)
      vhpi_error(vhpiError, &(obj->loc), "class kind %s is not a declaration",
                 vhpi_class_str(obj->kind));
   return decl;
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
      vhpi_error(vhpiError, &(obj->loc), "class kind %s is not an object "
                 "declaration", vhpi_class_str(obj->kind));
   return od;
}

static c_interfaceDecl *is_interfaceDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiPortDeclK:
   case vhpiGenericDeclK:
   case vhpiConstParamDeclK:
   case vhpiSigParamDeclK:
   case vhpiVarParamDeclK:
      return container_of(obj, c_interfaceDecl, objDecl.decl.object);
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
      return container_of(obj, c_subpDecl, object);
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
      vhpi_error(vhpiError, &(obj->loc), "class kind %s is not an expression",
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

static c_prefixedName *cast_prefixedName(c_vhpiObject *obj)
{
   c_prefixedName *pn = is_prefixedName(obj);
   if (pn == NULL)
      vhpi_error(vhpiError, &(obj->loc), "class kind %s is not a prefixed name",
                 vhpi_class_str(obj->kind));
   return pn;
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
      vhpi_error(vhpiError, &(obj->loc), "class kind %s is not an instance of "
                 "a design unit", vhpi_class_str(obj->kind));
      return NULL;
   }
}

static const char *handle_pp(vhpiHandleT handle)
{
   static __thread text_buf_t *tb = NULL;

   if (handle == NULL)
      return "NULL";

   if (tb == NULL)
      tb = tb_new();
   else
      tb_rewind(tb);

   tb_printf(tb, "%p:{", handle);

   handle_slot_t *slot = decode_handle(vhpi_context(), handle);
   if (slot == NULL)
      tb_cat(tb, "INVALID");
   else {
      c_vhpiObject *obj = slot->obj;
      tb_cat(tb, vhpi_class_str(obj->kind));

      c_abstractDecl *decl = is_abstractDecl(obj);
      if (decl != NULL)
         tb_printf(tb, " Name=%s", decl->Name);

      c_name *n = is_name(obj);
      if (n != NULL)
         tb_printf(tb, " Name=%s", n->Name);

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
   }

   tb_append(tb, '}');

   return tb_get(tb);
}

static const char *cb_data_pp(const vhpiCbDataT *data)
{
   static char buf[256];
   checked_sprintf(buf, sizeof(buf), "{reason=%s cb_rtn=%p user_data=%p}",
                   vhpi_cb_reason_str(data->reason), data->cb_rtn,
                   data->user_data);
   return buf;
}

static void *new_object(size_t size, vhpiClassKindT class)
{
   assert(size >= sizeof(c_vhpiObject));

   c_vhpiObject *obj = pool_calloc(vhpi_context()->pool, size);
   obj->kind = class;
   obj->loc = LOC_INVALID;

   return obj;
}

static void *recyle_object(size_t size, vhpiClassKindT class)
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
         obj->loc = LOC_INVALID;

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
   list->items = pool_malloc_array(mp, num, sizeof(c_vhpiObject));
}

static inline void vhpi_list_add(vhpiObjectListT *list, c_vhpiObject *obj)
{
   assert(list->count < list->limit);
   list->items[list->count++] = obj;
}

static void init_abstractRegion(c_abstractRegion *r, c_abstractRegion *upper,
                                tree_t t)
{
   const loc_t *loc = tree_loc(t);
   r->object.loc = *loc;

   r->UpperRegion = upper;
   r->LineNo      = loc->first_line;
   r->LineOffset  = loc->line_delta;

   if (is_design_unit(t)) {
      ident_t qual = tree_ident(t);
      ident_t suffix = ident_rfrom(qual, '.');

      r->Name = r->CaseName = new_string(istr(suffix));
      r->FullName = r->FullCaseName = new_stringf("@%s", istr(qual));
   }
   else {
      r->Name = r->CaseName = new_string(istr(tree_ident(t)));
      r->FullName = r->FullCaseName = new_stringf(":%s", r->Name);
   }

   r->tree   = t;
   r->handle = JIT_HANDLE_INVALID;
}

static void init_designInstUnit(c_designInstUnit *iu, c_abstractRegion *upper,
                                tree_t t, c_designUnit *u)
{
   init_abstractRegion(&(iu->region), upper, t);

   iu->DesignUnit = u;
}

static void init_abstractDecl(c_abstractDecl *d, tree_t t, c_abstractRegion *r)
{
   const loc_t *loc = tree_loc(t);
   d->object.loc = *loc;

   d->LineNo     = loc->first_line;
   d->LineOffset = loc->line_delta;

   d->Name = d->CaseName = new_string(istr(tree_ident(t)));
   if (r != NULL)
      d->FullName = d->FullCaseName =
         new_stringf("%s:%s", r->FullName, d->Name);

   d->ImmRegion = r;

   d->type = tree_type(t);
   d->tree = t;
}

static void init_objDecl(c_objDecl *d, tree_t t, c_abstractRegion *ImmRegion)
{
   init_abstractDecl(&(d->decl), t, ImmRegion);

   d->Type = cached_typeDecl(tree_type(t), &(d->decl.object));

   if (d->Type->IsUnconstrained) {
      void *ptr = vhpi_get_value_ptr(&(d->decl.object));
      if (ptr != NULL) {
         vhpiClassKindT kind = vhpi_get_prefix_kind(&(d->decl.object));
         d->Type = build_dynamicSubtype(d->Type, ptr, kind);
      }
   }

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

static void init_subpDecl(c_subpDecl *d, tree_t t)
{
   d->object.loc = *tree_loc(t);
   d->tree = t;

   tree_locus(t, &d->module, &d->offset);
}

static void init_elemDecl(c_elemDecl *ed, tree_t t, c_typeDecl *Type,
                          c_recordTypeDecl *parent)
{
   init_abstractDecl(&(ed->decl), t, parent->composite.typeDecl.decl.ImmRegion);
   ed->Type = Type;
   ed->Position = tree_pos(t);
   ed->parent = parent;
}

static void init_typeDecl(c_typeDecl *d, tree_t t, type_t type)
{
   init_abstractDecl(&(d->decl), t, NULL);

   char *full LOCAL = xasprintf("@%s", istr(type_ident(type)));
   char *pos = full;
   while ((pos = strchr(pos, '.')))
      *pos = ':';
   d->decl.FullName = d->decl.FullCaseName = new_string(full);

   d->type   = type;
   d->format = vhpi_format_for_type(d->type, &d->map_str);
   d->IsUnconstrained = type_is_unconstrained(d->type);
   d->homogeneous = type_is_homogeneous(d->type);
   d->numElems = d->IsUnconstrained ? -1 : 1;
}

static void init_scalarTypeDecl(c_scalarTypeDecl *d, tree_t t, type_t type)
{
   init_typeDecl(&(d->typeDecl), t, type);
   d->typeDecl.IsScalar = true;
   d->typeDecl.size = type_bit_width(type) / 8;
}

static void init_compositeTypeDecl(c_compositeTypeDecl *d, tree_t t, type_t type)
{
   init_typeDecl(&(d->typeDecl), t, type);
   d->typeDecl.IsComposite = true;
}

static void init_enumLiteral(c_enumLiteral *el, tree_t t, c_enumTypeDecl *Type)
{
   init_abstractDecl(&(el->decl), t, NULL);
   el->Type = Type;
   el->Position = tree_pos(t);
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
   n->Name = n->CaseName = Name;
   n->FullName = n->FullCaseName = FullName;
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
      Name = new_stringf("%s%s", name->Name, suffix);
      FullName = new_stringf("%s%s", name->FullName, suffix);
   }

   c_objDecl *obj = is_objDecl(prefix);
   if (obj != NULL) {
      Name = new_stringf("%s%s", obj->decl.Name, suffix);
      FullName = new_stringf("%s%s", obj->decl.FullName, suffix);
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
      if (ir->LeftBound > ir->RightBound) {
         idx = ir->LeftBound - indices[i];
         BaseIndex *= ir->LeftBound - ir->RightBound + 1;
      }
      else {
         idx = indices[i] - ir->LeftBound;
         BaseIndex *= ir->RightBound - ir->LeftBound + 1;
      }

      BaseIndex += indices[i];
      tb_printf(suffix, "%d", idx);
   }
   tb_append(suffix, ')');

   init_prefixedName(&(in->prefixedName), Type, prefix, tb_get(suffix));
   in->BaseIndex = BaseIndex;

   in->offset = BaseIndex * Type->numElems;
   if (prefix) {
      c_indexedName *pin = is_indexedName(prefix);
      if (pin)
         in->offset += pin->offset;
   }
}

static vhpiIntT range_len(c_intRange *ir)
{
   if (ir->range.IsUp)
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
   tb_cat(suffix, (const char *)Suffix->decl.Name);

   init_prefixedName(&(sn->prefixedName), Suffix->Type, prefix, tb_get(suffix));
   sn->Suffix = Suffix;
}

static void init_designUnit(c_designUnit *u, tree_t t)
{
   init_abstractRegion(&(u->region), NULL, t);

   ident_t uname = tree_ident(t);
   u->UnitName = new_string(istr(uname));
   u->Name = u->CaseName = new_string(istr(ident_rfrom(uname, '.')));
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

static vhpiObjectListT *expand_lazy_list(c_vhpiObject *obj, vhpiLazyListT *lazy)
{
   vhpiLazyFnT fn = lazy->fn;
   if (fn != NULL) {
      lazy->fn = NULL;   // Avoid infinite recursion
      (*fn)(obj);
   }

   return &(lazy->list);
}

static bool init_iterator(c_iterator *it, vhpiOneToManyT type,
                          c_vhpiObject *obj)
{
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
         it->list = expand_lazy_list(obj, &(region->decls));
         return true;
      case vhpiInternalRegions:
         it->filter = (vhpiFilterT)is_abstractRegion;
         it->list = expand_lazy_list(obj, &(region->stmts));
         return true;
      case vhpiConstDecls:
         it->filter = (vhpiFilterT)is_constDecl;
         it->list = expand_lazy_list(obj, &(region->decls));
         return true;
      case vhpiVarDecls:
         it->filter = (vhpiFilterT)is_varDecl;
         it->list = expand_lazy_list(obj, &(region->decls));
         return true;
      case vhpiSigDecls:
         it->filter = (vhpiFilterT)is_sigDecl;
         it->list = expand_lazy_list(obj, &(region->decls));
         return true;
      case vhpiGenericDecls:
         it->filter = (vhpiFilterT)is_genericDecl;
         it->list = expand_lazy_list(obj, &(region->decls));
         return true;
      case vhpiPortDecls:
         it->filter = (vhpiFilterT)is_portDecl;
         it->list = expand_lazy_list(obj, &(region->decls));
         return true;
      case vhpiStmts:
         it->list = expand_lazy_list(obj, &(region->stmts));
         return true;
      case vhpiBlockStmts:
         it->filter = (vhpiFilterT)is_blockStmt;
         it->list = expand_lazy_list(obj, &(region->stmts));
         return true;
      case vhpiCompInstStmts:
         it->filter = (vhpiFilterT)is_compInstStmt;
         it->list = expand_lazy_list(obj, &(region->stmts));
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

   c_arrayTypeDecl *array = is_arrayTypeDecl(obj);
   if (array != NULL) {
      if (type == vhpiConstraints) {
         it->list = &(array->Constraints);
         return true;
      }
      return false;
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
         it->list = &(subtype->Constraints);
         return true;
      }
      return false;
   }

   c_physTypeDecl *ptd = is_physTypeDecl(obj);
   if (ptd != NULL) {
      if (type == vhpiConstraints) {
         it->single = &(ptd->constraint->object);
         return true;
      }
      return false;
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

static void vhpi_signal_event_cb(uint64_t now, rt_signal_t *signal,
                                 rt_watch_t *watch, void *user)
{
   c_vhpiObject *obj = from_handle(user);
   if (obj == NULL)
      return;

   c_callback *cb = is_callback(obj);
   if (cb == NULL)
      return;

   vhpiTimeT time;
   if (cb->data.time != NULL) {
      vhpi_get_time(&time, NULL);
      cb->data.time = &time;
   }

   if (cb->State == vhpiEnable)
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

static rt_scope_t *vhpi_get_scope_abstractRegion(c_abstractRegion *region)
{
   if (region->scope)
      return region->scope;

   rt_scope_t *scope = find_scope(vhpi_context()->model, region->tree);
   if (scope == NULL) {
      vhpi_error(vhpiError, &(region->object.loc),
                 "cannot find scope object for %s", region->Name);
      return NULL;
   }

   region->scope = scope;
   return scope;
}

static rt_scope_t *vhpi_get_scope_objDecl(c_objDecl *decl)
{
   if (decl->scope)
      return decl->scope;

   rt_scope_t *parent = vhpi_get_scope_abstractRegion(decl->decl.ImmRegion);
   if (parent == NULL)
      return NULL;

   decl->scope = child_scope(parent, decl->decl.tree);
   if (decl->scope == NULL)
      vhpi_error(vhpiError, &(decl->decl.object.loc),
                 "cannot find scope object for %s", decl->decl.Name);

   return decl->scope;
}

static rt_scope_t *vhpi_get_scope_prefixedName(c_prefixedName *pn)
{
   if (pn->scope)
      return pn->scope;

   rt_scope_t *parent = NULL;

   c_prefixedName *ppn = is_prefixedName(pn->Prefix);
   if (ppn != NULL)
      parent = vhpi_get_scope_prefixedName(ppn);

   c_objDecl *od = is_objDecl(pn->Prefix);
   if (od != NULL)
      parent = vhpi_get_scope_objDecl(od);

   if (parent == NULL)
      return NULL;

   c_vhpiObject *obj = &(pn->name.expr.object);
   c_indexedName *in = is_indexedName(obj);
   c_selectedName *sn = is_selectedName(obj);
   if (in != NULL)
      pn->scope = child_scope_at(parent, in->BaseIndex);
   else if (sn != NULL)
      pn->scope = child_scope(parent, sn->Suffix->decl.tree);
   else
      fatal_trace("class kind %s not supported in %s",
                  vhpi_class_str(obj->kind), __func__);

   if (pn->scope == NULL)
      vhpi_error(vhpiError, &(obj->loc), "cannot find scope object for %s",
                 pn->name.Name);
   return pn->scope;
}

static rt_signal_t *vhpi_get_signal_objDecl(c_objDecl *decl)
{
   if (decl->signal != NULL)
      return decl->signal;

   rt_scope_t *scope = vhpi_get_scope_abstractRegion(decl->decl.ImmRegion);
   if (scope == NULL)
      return NULL;

   rt_signal_t *signal = find_signal(scope, decl->decl.tree);
   if (signal == NULL) {
      vhpi_error(vhpiError, &(decl->decl.object.loc),
                 "cannot find signal object for %s", decl->decl.Name);
      return NULL;
   }

   decl->signal = signal;
   return signal;
}

static rt_signal_t *vhpi_get_signal_prefixedName(c_prefixedName *pn)
{
   if (pn->signal)
      return pn->signal;

   rt_scope_t *scope = NULL;

   c_prefixedName *ppn = is_prefixedName(pn->Prefix);
   if (ppn != NULL) {
      if (ppn->name.expr.Type->homogeneous)
         pn->signal = vhpi_get_signal_prefixedName(ppn);
      else
         scope = vhpi_get_scope_prefixedName(ppn);
   }

   c_objDecl *od = is_objDecl(pn->Prefix);
   if (od != NULL) {
      if (od->Type->homogeneous)
         pn->signal = vhpi_get_signal_objDecl(od);
      else
         scope = vhpi_get_scope_objDecl(od);
   }

   c_vhpiObject *obj = &(pn->name.expr.object);
   if (scope) {
      c_selectedName *sn = is_selectedName(obj);
      assert(sn != NULL);

      pn->signal = find_signal(scope, sn->Suffix->decl.tree);
   }

   if (pn->signal == NULL)
      vhpi_error(vhpiError, &(obj->loc), "cannot find signal object for %s",
                 pn->name.Name);
   return pn->signal;
}

static c_typeDecl *vhpi_get_type(c_vhpiObject *obj)
{
   c_objDecl *od = is_objDecl(obj);
   if (od != NULL)
      return od->Type;

   c_expr *expr = is_expr(obj);
   if (expr != NULL)
      return expr->Type;

   c_elemDecl *ed = is_elemDecl(obj);
   if (ed != NULL)
      return ed->Type;

   c_typeDecl *td = is_typeDecl(obj);
   if (td != NULL)
      return td;

   return NULL;
}

static vhpiClassKindT vhpi_get_prefix_kind(c_vhpiObject *obj)
{
   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL)
      return vhpi_get_prefix_kind(pn->Prefix);
   else
      return obj->kind;
}

static void *vhpi_get_bounds_var(c_typeDecl *td)
{
   jit_t *j = vhpi_context()->jit;

   ident_t id = type_ident(td->type);
   jit_handle_t handle = jit_lazy_compile(j, ident_runtil(id, '.'));

   return jit_get_frame_var(j, handle, id);
}

static const jit_layout_t *vhpi_get_layout(c_vhpiObject *obj)
{
   c_typeDecl *td = vhpi_get_type(obj);
   if (td == NULL)
      return NULL;

   switch (vhpi_get_prefix_kind(obj)) {
   case vhpiPortDeclK:
   case vhpiSigDeclK:
      return signal_layout_of(td->type);
   default:
      return layout_of(td->type);
   }
}

static void *vhpi_get_value_ptr(c_vhpiObject *obj)
{
   jit_t *j = vhpi_context()->jit;

   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL) {
      void *base = vhpi_get_value_ptr(pn->Prefix);
      if (base == NULL)
         return NULL;

      const jit_layout_t *l = vhpi_get_layout(pn->Prefix);
      if (l == NULL)
         return NULL;

      c_selectedName *sn = is_selectedName(obj);
      if (sn != NULL) {
         assert(sn->Suffix->Position < l->nparts);
         return base + l->parts[sn->Suffix->Position].offset;
      }

      c_indexedName *in = is_indexedName(obj);
      if (in != NULL) {
         if (l->parts[0].class == LC_EXTERNAL)
            return ((ffi_uarray_t *)base)->ptr;   // Wrapped array
         else
            return base;
      }

      vhpi_error(vhpiInternal, &(obj->loc), "unsupported prefixed name kind %s",
                 vhpi_class_str(obj->kind));
      return NULL;
   }

   c_recordTypeDecl *rtd = is_recordTypeDecl(obj);
   if (rtd != NULL) {
      void *base = vhpi_get_bounds_var(&(rtd->composite.typeDecl));
      if (base != NULL)
         return base;

      vhpi_error(vhpiInternal, &(obj->loc), "missing bounds variable for %s",
                 type_pp(rtd->composite.typeDecl.type));
      return NULL;
   }

   c_elemDecl *ed = is_elemDecl(obj);
   if (ed != NULL) {
      c_vhpiObject *pobj = &(ed->parent->composite.typeDecl.decl.object);
      void *base = vhpi_get_value_ptr(pobj);
      if (base == NULL)
         return NULL;

      const jit_layout_t *l = vhpi_get_layout(pobj);
      assert(l != NULL);
      assert(ed->Position < l->nparts);

      return base + l->parts[ed->Position].offset;
   }

   c_objDecl *decl = is_objDecl(obj);
   assert(decl != NULL);

   if (decl->decl.ImmRegion == NULL)
      return NULL;
   else if (decl->decl.ImmRegion->handle == JIT_HANDLE_INVALID) {
      c_packInst *pi = is_packInst(&(decl->decl.ImmRegion->object));
      if (pi != NULL) {
         ident_t qual = tree_ident(decl->decl.ImmRegion->tree);
         if (pi->designInstUnit.region.UpperRegion != NULL) {
            for (c_abstractRegion *it = pi->designInstUnit.region.UpperRegion;
                 it != NULL; it = it->UpperRegion)
               qual = ident_prefix(tree_ident(it->tree), qual, '.');

            qual = ident_prefix(lib_name(lib_work()), qual, '.');
         }

         decl->decl.ImmRegion->handle = jit_lazy_compile(j, qual);

         if (jit_link(j, decl->decl.ImmRegion->handle) == NULL) {
            vhpi_error(vhpiError, &(obj->loc), "failed to initialise package");
            return NULL;
         }
      }
      else {
         rt_scope_t *scope =
            vhpi_get_scope_abstractRegion(decl->decl.ImmRegion);

         if (*mptr_get(scope->privdata) == NULL) {
            vhpi_error(vhpiError, &(obj->loc), "%s has not been elaborated",
                       decl->decl.FullName);
            return NULL;
         }

         decl->decl.ImmRegion->handle = jit_lazy_compile(j, scope->name);
      }
   }

   ident_t name = tree_ident(decl->decl.tree);
   return jit_get_frame_var(j, decl->decl.ImmRegion->handle, name);
}

static void vhpi_get_uarray(c_vhpiObject *obj, void **ptr, ffi_dim_t **dims)
{
   if (ptr) *ptr = NULL;
   if (dims) *dims = NULL;

   void *base = vhpi_get_value_ptr(obj);
   if (base == NULL)
      return;

   if (ptr != NULL)
      *ptr = *(void **)base;

   if (dims != NULL) {
      switch (vhpi_get_prefix_kind(obj)) {
      case vhpiSigDeclK:
      case vhpiPortDeclK:
         // Signals have extra word for offset
         *dims = base + 2*sizeof(int64_t);
         break;
      default:
         *dims = base + sizeof(int64_t);
         break;
      }
   }
}

static int vhpi_count_subsignals(rt_model_t *m, rt_scope_t *s)
{
   assert(is_signal_scope(s));

   int count = s->signals.count;
   for (int i = 0; i < s->children.count; i++)
      count += vhpi_count_subsignals(m, s->children.items[i]);

   return count;
}

static void vhpi_watch_scope(rt_model_t *m, rt_scope_t *s, rt_watch_t *w)
{
   assert(is_signal_scope(s));

   for (int i = 0; i < s->signals.count; i++)
      model_set_event_cb(m, s->signals.items[i], w);

   for (int i = 0; i < s->children.count; i++)
      vhpi_watch_scope(m, s->children.items[i], w);
}

////////////////////////////////////////////////////////////////////////////////
// Public API

DLLEXPORT
int vhpi_release_handle(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(handle));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL) {
      vhpi_error(vhpiError, NULL, "invalid handle %p", handle);
      return 1;
   }

   drop_handle(vhpi_context(), handle);
   return 0;
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
      {
         c_callback *cb = recyle_object(sizeof(c_callback), vhpiCallbackK);
         cb->Reason  = cb_data_p->reason;
         cb->State   = (flags & vhpiDisableCb) ? vhpiDisable : vhpiEnable;
         cb->data    = *cb_data_p;

         APUSH(vhpi_context()->callbacks, handle_for(&(cb->object)));

         return (flags & vhpiReturnCb) ? handle_for(&(cb->object)) : NULL;
      }

   case vhpiCbAfterDelay:
      {
         if (cb_data_p->time == NULL) {
            vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
            return NULL;
         }

         c_callback *cb = recyle_object(sizeof(c_callback), vhpiCallbackK);
         cb->Reason  = cb_data_p->reason;
         cb->State   = (flags & vhpiDisableCb) ? vhpiDisable : vhpiEnable;
         cb->data    = *cb_data_p;

         const uint64_t now = model_now(m, NULL);
         const uint64_t when = vhpi_time_to_native(cb_data_p->time) + now;

         vhpiHandleT handle = handle_for(&(cb->object));
         model_set_timeout_cb(m, when, vhpi_global_cb, handle);

         return (flags & vhpiReturnCb) ? handle_for(&(cb->object)) : NULL;
      }

   case vhpiCbValueChange:
      {
         c_vhpiObject *obj = from_handle(cb_data_p->obj);
         if (obj == NULL)
            return NULL;

         rt_signal_t *signal = NULL;
         rt_scope_t *scope = NULL;

         c_prefixedName *pn;
         c_objDecl *decl;
         if ((pn = is_prefixedName(obj))) {
            if ((signal = vhpi_get_signal_prefixedName(pn)) == NULL)
               return NULL;
         }
         else if ((decl = is_objDecl(obj))) {
            if (decl->Type->homogeneous) {
               if ((signal = vhpi_get_signal_objDecl(decl)) == NULL)
                  return NULL;
            }
            else if ((scope = vhpi_get_scope_objDecl(decl)) == NULL)
               return NULL;
         }
         else {
            vhpi_error(vhpiInternal, &(obj->loc), "cannot register value "
                       "callback for kind %s", vhpi_class_str(obj->kind));
            return NULL;
         }

         c_callback *cb = recyle_object(sizeof(c_callback), vhpiCallbackK);
         cb->Reason = cb_data_p->reason;
         cb->State  = (flags & vhpiDisableCb) ? vhpiDisable : vhpiEnable;
         cb->data   = *cb_data_p;

         const int slots = scope != NULL ? vhpi_count_subsignals(m, scope) : 1;

         vhpiHandleT handle = handle_for(&(cb->object));
         cb->watch = watch_new(m, vhpi_signal_event_cb, handle,
                               WATCH_EVENT, slots);

         if (signal != NULL)
            cb->watch = model_set_event_cb(m, signal, cb->watch);
         else
            vhpi_watch_scope(m, scope, cb->watch);

         return (flags & vhpiReturnCb) ? handle_for(&(cb->object)) : NULL;
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

   if (cb->Reason == vhpiCbValueChange)
      watch_free(c->model, cb->watch);

   cb->State = vhpiMature;

   // Two references are created in vhpi_register_cb: one for the
   // internal callback and one to return to the user
   drop_handle(c, handle);
   drop_handle(c, handle);

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
      vhpi_error(vhpiWarning, &(obj->loc),
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
      vhpi_error(vhpiWarning, &(obj->loc),
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
      return handle_for(&(vhpi_context()->root->designInstUnit.region.object));
   case vhpiTool:
      return handle_for(&(vhpi_context()->tool->object));
   default:
      break;
   }

   c_vhpiObject *obj = from_handle(referenceHandle);
   if (obj == NULL)
      return NULL;

   switch (type) {
   case vhpiBaseType:
      {
         c_typeDecl *td = is_typeDecl(obj);
         if (td != NULL) {
            td = td->BaseType ?: td;
            return handle_for(&(td->decl.object));
         }

         c_objDecl *od = is_objDecl(obj);
         if (od != NULL) {
            td = od->Type->BaseType ?: od->Type;
            return handle_for(&(td->decl.object));
         }

         c_expr *e = is_expr(obj);
         if (e != NULL) {
            td = e->Type->BaseType ?: e->Type;
            return handle_for(&(td->decl.object));
         }

         c_enumLiteral *el = is_enumLiteral(obj);
         if (el != NULL) {
            td = el->Type->scalar.typeDecl.BaseType ?:
               &(el->Type->scalar.typeDecl);
            return handle_for(&(td->decl.object));
         }

         c_elemDecl *ed = is_elemDecl(obj);
         if (ed != NULL) {
            td = ed->Type->BaseType ?: ed->Type;
            return handle_for(&(td->decl.object));
         }
      }
      break;

   case vhpiType:
   case DEPRECATED_vhpiSubtype:
      {
         c_objDecl *od = is_objDecl(obj);
         if (od != NULL)
            return handle_for(&(od->Type->decl.object));

         c_elemDecl *ed = is_elemDecl(obj);
         if (ed != NULL)
            return handle_for(&(ed->Type->decl.object));

         c_expr *e = is_expr(obj);
         if (e != NULL)
            return handle_for(&(e->Type->decl.object));
      }
      break;

   case vhpiElemType:
      {
         c_arrayTypeDecl *a = cast_arrayTypeDecl(obj);
         if (a == NULL)
            return NULL;
         return handle_for(&(a->ElemType->decl.object));
      }

   case vhpiDesignUnit:
      {
         c_designInstUnit *iu = cast_designInstUnit(obj);
         if (iu == NULL)
            return NULL;

         return handle_for(&(iu->DesignUnit->region.object));
      }

   case vhpiPrimaryUnit:
      {
         c_secondaryUnit *su = cast_secondaryUnit(obj);
         if (su == NULL)
            return NULL;

         return handle_for(&(su->PrimaryUnit->region.object));
      }

   case vhpiPrefix:
      {
         c_prefixedName *pn = cast_prefixedName(obj);
         if (pn == NULL)
            return NULL;

         return handle_for(pn->Prefix);
      }
   case vhpiSuffix:
      {
         c_selectedName *sn = cast_selectedName(obj);
         if (sn == NULL)
            return NULL;

         return handle_for(&(sn->Suffix->decl.object));
      }
   case vhpiParamDecl:
      {
         c_forGenerate *g = cast_forGenerate(obj);
         if (g == NULL)
            return NULL;

         return handle_for(&(g->ParamDecl->objDecl.decl.object));
      }
   case DEPRECATED_vhpiReturnTypeMark:
   case DEPRECATED_vhpiName:
   case DEPRECATED_vhpiTypeMark:
   case DEPRECATED_vhpiDecl:
      vhpi_error(vhpiError, &(obj->loc), "relationship %s is deprecated and "
                 "not implemented in vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;
   default:
      vhpi_error(vhpiInternal, &(obj->loc), "relationship %s not supported in "
                 "vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;
   }

   vhpi_error(vhpiError, &(obj->loc), "invalid relationship %s for handle %s",
              vhpi_one_to_one_str(type), handle_pp(referenceHandle));
   return NULL;
}

DLLEXPORT
vhpiHandleT vhpi_handle_by_name(const char *name, vhpiHandleT scope)
{
   vhpi_clear_error();

   VHPI_TRACE("name=%s scope=%p", name, scope);

   char *copy LOCAL = xstrdup(name), *saveptr;
   char *elem = strtok_r(copy, ":.", &saveptr);

   c_vhpiObject *where = NULL;
   if (scope == NULL) {
      vhpi_context_t *c = vhpi_context();

      if (strcasecmp(elem, (char *)c->root->designInstUnit.region.Name) == 0)
         where = &(c->root->designInstUnit.region.object);
      else {
         for (int i = 0; i < c->packages.count; i++) {
            c_packInst *pi = is_packInst(c->packages.items[i]);
            assert(pi != NULL);

            if (strcasecmp(elem, (char *)pi->designInstUnit.region.Name) == 0) {
               where = &(pi->designInstUnit.region.object);
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
         vhpiObjectListT *decls = expand_lazy_list(where, &(region->decls));
         for (int i = 0; i < decls->count; i++) {
            c_abstractDecl *d = cast_abstractDecl(decls->items[i]);
            if (strcasecmp((char *)d->Name, elem) == 0) {
               where = &(d->object);
               found = true;
               break;
            }
         }

         if (!found) {
            vhpiObjectListT *stmts = expand_lazy_list(where, &(region->stmts));
            for (int i = 0; i < stmts->count; i++) {
               c_abstractRegion *r = is_abstractRegion(stmts->items[i]);
               if (r != NULL && strcasecmp((char *)r->Name, elem) == 0) {
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

            if (strcasecmp((char *)sn->Suffix->decl.Name, elem) == 0) {
               where = &(sn->prefixedName.name.expr.object);
               found = true;
               break;
            }
         }
      }

      if (!found) {
         vhpi_error(vhpiError, &(where->loc), "suffix %s not found in prefix "
                    "of class %s", elem, vhpi_class_str(where->kind));
         return NULL;
      }
   }

   return handle_for(where);
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
      vhpi_error(vhpiError, obj ? &(obj->loc) : NULL,
                 "relation %s not supported for parent %s",
                 vhpi_one_to_many_str(itRel), handle_pp(parent));
      return NULL;
   }

   if (it.single ? index : index >= it.list->count) {
      vhpi_error(vhpiError, obj ? &(obj->loc) : NULL, "invalid %s index %d",
                 vhpi_one_to_many_str(itRel), index);
      return NULL;
   }

   return handle_for(it.single ?: it.list->items[index]);
}

DLLEXPORT
vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT handle)
{
   VHPI_TRACE("type=%s handle=%s", vhpi_one_to_many_str(type),
              handle_pp(handle));

   c_vhpiObject *obj = NULL;
   if (handle != NULL && (obj = from_handle(handle)) == NULL)
      return NULL;

   c_iterator *it = recyle_object(sizeof(c_iterator), vhpiIteratorK);
   if (!init_iterator(it, type, obj)) {
      vhpi_error(vhpiError, obj ? &(obj->loc) : NULL,
                 "relation %s not supported for handle %s",
                 vhpi_one_to_many_str(type), handle_pp(handle));
      return NULL;
   }

   return handle_for(&(it->object));
}

DLLEXPORT
vhpiHandleT vhpi_scan(vhpiHandleT iterator)
{
   VHPI_TRACE("handle=%s", handle_pp(iterator));

   c_vhpiObject *obj = from_handle(iterator);
   if (obj == NULL)
      return NULL;

   c_iterator *it = cast_iterator(obj);
   if (it == NULL)
      return NULL;

   if (it->single)
      return it->pos++ ? NULL : handle_for(it->single);

   while (it->pos < it->list->count) {
      c_vhpiObject *obj = it->list->items[it->pos++];
      if (it->filter == NULL || (*it->filter)(obj) != NULL)
         return handle_for(obj);
   }

   return NULL;
}

DLLEXPORT
vhpiIntT vhpi_get(vhpiIntPropertyT property, vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              handle_pp(handle));

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
         c_typeDecl *td = NULL;
         c_name *n = is_name(obj);
         if (n != NULL)
            td = n->expr.Type;
         else {
            c_objDecl *decl = is_objDecl(obj);
            if (decl != NULL)
               td = decl->Type;
         }

         if (td == NULL)
            goto missing_property;

         assert(td->numElems >= 0);
         return td->numElems;
      }

   case vhpiArgcP:
      {
         c_tool *t = cast_tool(obj);
         if (t == NULL)
            return vhpiUndefined;
         return t->argv.count;
      }

   case vhpiNumDimensionsP:
      {
         c_arrayTypeDecl *a = is_arrayTypeDecl(obj);
         if (a != NULL)
            return a->NumDimensions;

         c_subTypeDecl *s = is_subTypeDecl(obj);
         if (s != NULL
             && s->typeDecl.BaseType->decl.object.kind == vhpiArrayTypeDeclK)
            return s->Constraints.count;

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
         c_intRange *ir = cast_intRange(obj);
         if (ir == NULL)
            goto missing_property;

         if (ir->range.IsUnconstrained) {
            vhpi_error(vhpiError, &(obj->loc), "cannot get bounds of "
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
         if (e == NULL)
            goto missing_property;

         return e->Staticness;
      }

   case vhpiBaseIndexP:
      {
         c_indexedName *in = cast_indexedName(obj);
         if (in == NULL)
            return vhpiUndefined;

         return in->BaseIndex;
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

         c_enumLiteral *el = cast_enumLiteral(obj);
         if (el == NULL)
            goto missing_property;

         return el->Position;
      }

   case vhpiNumFieldsP:
      {
         c_recordTypeDecl *rtd = cast_recordTypeDecl(obj);
         if (rtd == NULL)
            goto missing_property;

         return expand_lazy_list(obj, &rtd->RecordElems)->count;
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

   default:
      vhpi_error(vhpiFailure, &(obj->loc), "unsupported property %s in "
                 "vhpi_get", vhpi_property_str(property));
      return vhpiUndefined;
   }

missing_property:
   vhpi_error(vhpiError, &(obj->loc), "object does not have property %s",
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

   if (property == vhpiKindStrP)
      return (vhpiCharT *)vhpi_class_str(obj->kind);

   c_designUnit *u = is_designUnit(obj);
   if (u != NULL) {
      switch (property) {
      case vhpiUnitNameP: return u->UnitName;
      default: break;   // Fallthrough to c_abstractRegion
      }
   }

   c_abstractRegion *region = is_abstractRegion(obj);
   if (region != NULL) {
      switch (property) {
      case vhpiNameP: return region->Name;
      case vhpiCaseNameP: return region->CaseName;
      case vhpiFileNameP: return (vhpiCharT *)loc_file_str(&(region->object.loc));
      case vhpiFullNameP: return region->FullName;
      case vhpiFullCaseNameP: return region->FullCaseName;
      default: break;   // May be statement instance
      }
   }

   c_enumLiteral *el = is_enumLiteral(obj);
   if (el != NULL) {
      switch (property) {
      case vhpiStrValP: return el->decl.Name;
      default: goto unsupported;
      }
   }

   c_abstractDecl *d = is_abstractDecl(obj);
   if (d != NULL) {
      switch (property) {
      case vhpiNameP: return d->Name;
      case vhpiCaseNameP: return d->CaseName;
      case vhpiFileNameP: return (vhpiCharT *)loc_file_str(&(d->object.loc));
      case vhpiFullNameP: return d->FullName;
      case vhpiFullCaseNameP: return d->FullCaseName;
      default: goto unsupported;
      }
   }

   c_name *n = is_name(obj);
   if (n != NULL) {
      switch (property) {
      case vhpiNameP: return n->Name;
      case vhpiCaseNameP: return n->CaseName;
      case vhpiFullNameP: return n->FullName;
      case vhpiFullCaseNameP: return n->FullCaseName;
      default: goto unsupported;
      }
   }

   c_tool *t = is_tool(obj);
   if (t != NULL) {
      switch (property) {
      case vhpiToolVersionP:
         return (vhpiCharT *)PACKAGE_VERSION;
      case vhpiNameP:
         return (vhpiCharT *)PACKAGE_NAME;
      default: goto unsupported;
      }
   }

   c_argv *arg = is_argv(obj);
   if (arg != NULL) {
      switch (property) {
      case vhpiStrValP: return arg->StrVal;
      default: goto unsupported;
      }
   }

   c_stmt *s = is_stmt(obj);
   if (s != NULL) {
      switch (property) {
      case vhpiLabelNameP: return s->LabelName;
      default: goto unsupported;
      }
   }

unsupported:
   vhpi_error(vhpiError, &(obj->loc), "object does not have string property %s",
              vhpi_property_str(property));
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
      vhpi_error(vhpiError, &(obj->loc), "invalid property %s in vhpi_get_real",
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

         c_typeDecl *t = decl->Type->BaseType ?: decl->Type;
         c_physTypeDecl *td = cast_physTypeDecl(&(t->decl.object));
         if (td == NULL)
            return invalid;

         rt_signal_t *signal = vhpi_get_signal_objDecl(decl);
         if (signal == NULL)
            return invalid;

         uint64_t value;
         signal_expand(signal, &value, 1);

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

   default:
      vhpi_error(vhpiError, &(obj->loc), "invalid property %s in vhpi_get_phys",
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

DLLEXPORT
int vhpi_get_value(vhpiHandleT expr, vhpiValueT *value_p)
{
   vhpi_clear_error();

   VHPI_TRACE("expr=%s value_p=%p", handle_pp(expr), value_p);

   c_vhpiObject *obj = from_handle(expr);
   if (obj == NULL)
      return -1;

   int offset = 0;
   c_objDecl *decl = NULL;
   c_typeDecl *td;
   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL) {
      td = pn->name.expr.Type;
      c_indexedName *in = is_indexedName(obj);
      if (in)
         offset = in->offset;
   }
   else {
      decl = cast_objDecl(obj);
      if (decl == NULL)
         return 1;
      td = decl->Type;
   }

   if (td->format == (vhpiFormatT)-1) {
      vhpi_error(vhpiInternal, &(obj->loc), "type %s not supported in "
                 "vhpi_get_value", type_pp(td->type));
      return -1;
   }

   int size = td->size, num_elems = td->numElems;
   const unsigned char *value = NULL;
   switch (vhpi_get_prefix_kind(obj)) {
   case vhpiGenericDeclK:
   case vhpiConstDeclK:
      {
         if (td->wrapped) {
            ffi_dim_t *dims;
            vhpi_get_uarray(obj, (void **)&value, &dims);
            assert(num_elems == ffi_array_length(dims[0].length));
         }
         else if ((value = vhpi_get_value_ptr(obj)) == NULL)
            return -1;
      }
      break;

   case vhpiSigDeclK:
   case vhpiPortDeclK:
      {
         rt_signal_t *signal;
         if (pn != NULL)
            signal = vhpi_get_signal_prefixedName(pn);
         else
            signal = vhpi_get_signal_objDecl(decl);

         if (signal == NULL)
            return -1;

         value = signal_value(signal);
         size = signal_size(signal);
      }
      break;

   case vhpiConstParamDeclK:
   case vhpiVarParamDeclK:
      {
         vhpi_context_t *c = vhpi_context();
         assert(c->args != NULL);

         c_interfaceDecl *id = is_interfaceDecl(obj);
         assert(id != NULL);

         if (td->wrapped) {
            value = c->args[id->argslot].pointer;
            num_elems = ffi_array_length(c->args[id->argslot + 2].integer);
         }
         else if (td->IsComposite) {
            value = c->args[id->argslot].pointer;
            num_elems = td->numElems;
         }
         else
            value = (void *)&(c->args[id->argslot]);
      }
      break;

   default:
      vhpi_error(vhpiError, &(obj->loc), "class kind %s cannot be used with "
                 "vhpi_get_value", vhpi_class_str(obj->kind));
      return -1;
   }

   assert(td->IsComposite || num_elems == 1);
   assert(num_elems >= 0);

   if (value_p->format == vhpiObjTypeVal)
      value_p->format = td->format;
   else if (value_p->format == vhpiBinStrVal && td->map_str != NULL)
      value_p->format = vhpiBinStrVal;
   else if (value_p->format != td->format
            && !vhpi_scalar_fits_format(value_p->format, size)) {
      vhpi_error(vhpiError, &(obj->loc), "invalid format %d for "
                 "object %s: expecting %d", value_p->format,
                 pn ? pn->name.Name : decl->decl.Name, td->format);
      return -1;
   }

   int64_t scalar = 0;
   switch (td->format) {
   case vhpiLogicVal:
   case vhpiSmallEnumVal:
   case vhpiCharVal:
      scalar = value[offset];
      break;
   case vhpiEnumVal:
#define READ_ENUM(type) scalar = ((const type *)value)[offset]
      FOR_ALL_SIZES(size, READ_ENUM);
      break;
   case vhpiIntVal:
      scalar = ((const vhpiIntT *)value)[offset];
      break;
   case vhpiLongIntVal:
      scalar = ((const vhpiLongIntT *)value)[offset];
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
      value_p->value.real = ((const double *)value)[offset];
      return 0;

   case vhpiLogicVecVal:
      {
         const int max = value_p->bufSize / sizeof(vhpiEnumT);
         if (max < num_elems)
            return num_elems * sizeof(vhpiEnumT);

         value_p->numElems = num_elems;

         const uint8_t *p = value + offset;
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

         const uint8_t *p = value + offset;
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

#define READ_ENUMV(type) do {                                   \
            const type *p = ((const type *)value) + offset;     \
            for (int i = 0; i < value_p->numElems; i++)         \
               value_p->value.enumvs[i] = *p++;                 \
         } while (0)

         FOR_ALL_SIZES(size, READ_ENUMV);
         return 0;
      }

   case vhpiBinStrVal:
   case vhpiStrVal:
      {
         if (value_p->bufSize < num_elems + 1)
            return num_elems + 1;

         value_p->numElems = num_elems;

         const vhpiCharT *p = value + offset;
         for (int i = 0; i < value_p->numElems; i++) {
            if (value_p->format == vhpiBinStrVal)
               value_p->value.str[i] = td->map_str[*p++];
            else
               value_p->value.str[i] = *p++;
         }

         value_p->value.str[value_p->numElems] = '\0';
         return 0;
      }

   case vhpiRealVecVal:
      {
         if (value_p->bufSize / sizeof(vhpiRealT) < num_elems)
            return num_elems * sizeof(vhpiRealT);

         value_p->numElems = num_elems;

         const double *p = ((const double *)value) + offset;
         for (int i = 0; i < value_p->numElems; i++)
            value_p->value.reals[i] = *p++;

         return 0;
      }

   case vhpiIntVecVal:
      {
         if (value_p->bufSize / sizeof(vhpiIntT) < num_elems)
            return num_elems * sizeof(vhpiIntT);

         value_p->numElems = num_elems;

#define READ_INTGS(type) do {                                   \
            const type *p = ((const type *)value) + offset;     \
            for (int i = 0; i < value_p->numElems; i++)         \
               value_p->value.intgs[i] = *p++;                  \
         } while (0)

         FOR_ALL_SIZES(size, READ_INTGS);

         return 0;
      }

   default:
      vhpi_error(vhpiError, &(obj->loc), "unsupported format %d",
                 value_p->format);
      return -1;
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

   int offset = 0;
   rt_signal_t *signal = NULL;
   c_typeDecl *td = NULL;
   c_prefixedName *pn = NULL;
   c_funcDecl *func = NULL;
   c_varParamDecl *vpd = NULL;
   if ((pn = is_prefixedName(obj))) {
      if ((signal = vhpi_get_signal_prefixedName(pn)) == NULL)
         return 1;

      c_indexedName *in = is_indexedName(obj);
      if (in)
         offset = in->offset;

      td = pn->name.expr.Type;
   }
   else if ((func = is_funcDecl(obj)))
      td = func->ReturnType;
   else if ((vpd = is_varParamDecl(obj)))
      td = vpd->interface.objDecl.Type;
   else {
      c_objDecl *decl = cast_objDecl(obj);
      if (decl == NULL)
         return 1;

      switch (obj->kind) {
      case vhpiSigDeclK:
      case vhpiPortDeclK:
         if ((signal = vhpi_get_signal_objDecl(decl)) == NULL)
            return 1;
         break;
      default:
         break;
      }

      td = decl->Type;
   }

   if (mode == vhpiSizeConstraint) {
      if (func == NULL) {
         vhpi_error(vhpiError, &(obj->loc), "vhpiSizeConstraint is only valid "
                    "for function result");
         return 1;
      }
      else if (!func->ReturnType->wrapped) {
         vhpi_error(vhpiError, &(obj->loc), "function return type does not "
                    "have a size constraint");
         return 1;
      }

      vhpi_context_t *c = vhpi_context();
      c->args[0].pointer = tlab_alloc(c->tlab, value_p->numElems * td->size);
      c->args[1].integer = 1;
      c->args[2].integer = value_p->numElems;

      return 0;
   }

   void *ext LOCAL = NULL, *ptr = NULL;
   union {
      uint8_t  uint8_t_val;
      uint16_t uint16_t_val;
      uint32_t uint32_t_val;
      uint64_t uint64_t_val;
      int64_t  int64_t_val;
      double   double_val;
   } scalar = { .uint64_t_val = 0 };
   int num_elems = 0;

   switch (value_p->format) {
   case vhpiLogicVal:
      num_elems = 1;
      scalar.uint8_t_val = value_p->value.enumv;
      ptr = &scalar;
      break;

   case vhpiSmallEnumVal:
      num_elems = 1;
      scalar.uint8_t_val = value_p->value.smallenumv;
      ptr = &scalar;
      break;

   case vhpiEnumVal:
      num_elems = 1;
      ptr = &scalar;

#define STORE_ENUM(type) do {                            \
         scalar.type##_val = value_p->value.enumv;       \
      } while (0)

      FOR_ALL_SIZES(td->size, STORE_ENUM);
      break;

   case vhpiCharVal:
      num_elems = 1;
      scalar.uint8_t_val = value_p->value.ch;
      ptr = &scalar;
      break;

   case vhpiIntVal:
      num_elems = 1;
      scalar.int64_t_val = value_p->value.intg;
      ptr = &scalar;   // Assume little endian
      break;

   case vhpiRealVal:
      num_elems = 1;
      scalar.double_val = value_p->value.real;
      ptr = &scalar;
      break;

   case vhpiLogicVecVal:
      num_elems = value_p->bufSize / sizeof(vhpiEnumT);
      ext = ptr = xmalloc(num_elems);
      for (int i = 0; i < num_elems; i++)
         ((uint8_t *)ext)[i] = value_p->value.enumvs[i];
      break;

   case vhpiSmallEnumVecVal:
      num_elems = value_p->bufSize / sizeof(vhpiSmallEnumT);
      ext = ptr = xmalloc(num_elems);
      for (int i = 0; i < num_elems; i++)
         ((uint8_t *)ext)[i] = value_p->value.smallenumvs[i];
      break;

   case vhpiEnumVecVal:
      {
         num_elems = value_p->bufSize / sizeof(vhpiEnumT);
         ext = ptr = xmalloc_array(num_elems, td->size);

#define STORE_ENUMV(type) do {                                  \
            for (int i = 0; i < num_elems; i++)                 \
               ((type *)ext)[i] = value_p->value.enumvs[i];     \
         } while (0)

         FOR_ALL_SIZES(td->size, STORE_ENUMV);
         break;
      }

   case vhpiStrVal:
      num_elems = value_p->bufSize - 1;
      ext = ptr = xmalloc(num_elems);
      for (int i = 0; i < num_elems; i++)
         ((vhpiCharT *)ext)[i] = value_p->value.str[i];
      break;

   case vhpiRealVecVal:
      {
         num_elems = value_p->bufSize / sizeof(vhpiRealT);
         ext = ptr = xmalloc_array(num_elems, sizeof(double));
         for (int i = 0; i < num_elems; i++)
            ((double *)ext)[i] = value_p->value.reals[i];
         break;
      }

   case vhpiIntVecVal:
      {
         num_elems = value_p->bufSize / sizeof(vhpiIntT);
         ext = ptr = xmalloc_array(num_elems, sizeof(vhpiIntT));

#define STORE_INTGS(type) do {                                          \
            for (int i = 0; i < num_elems; i++)                         \
               ((type *)ext)[i] = (int64_t)value_p->value.intgs[i];     \
         } while (0)

         FOR_ALL_SIZES(td->size, STORE_INTGS);
         break;
      }

   default:
      vhpi_error(vhpiFailure, &(obj->loc), "value format %d not supported "
                 "in vhpi_put_value", value_p->format);
      return 1;
   }

   if (signal != NULL) {
      rt_model_t *model = vhpi_context()->model;
      if (!model_can_create_delta(model)) {
         vhpi_error(vhpiError, &(obj->loc), "cannot create delta cycle "
                    "during current simulation phase");
         return 1;
      }
      else if (offset + num_elems > signal_width(signal)) {
         vhpi_error(vhpiError, &(obj->loc),
                    "too many values (%d) for signal with %d elements",
                    num_elems, signal_width(signal));
         return 1;
      }

      switch (mode) {
      case vhpiForcePropagate:
         force_signal(model, signal, ptr, offset, num_elems);
         return 0;
      case vhpiDepositPropagate:
         deposit_signal(model, signal, ptr, offset, num_elems);
         return 0;
      case vhpiRelease:
         release_signal(model, signal, offset, signal_width(signal));
         return 0;
      default:
         break;
      }
   }
   else if (func != NULL || vpd != NULL) {
      vhpi_context_t *c = vhpi_context();
      assert(c->args != NULL);

      const int slot = vpd ? vpd->interface.argslot : 0;

      switch (mode) {
      case vhpiForce:
      case vhpiForcePropagate:
      case vhpiDeposit:
      case vhpiDepositPropagate:
         if (td->IsScalar && vpd == NULL) {
            c->args[slot].integer = scalar.uint64_t_val;   // Function result
            return 0;
         }
         else if (td->IsScalar) {
#define PUT_SCALAR(type) *(type *)c->args[slot].pointer = scalar.type##_val;
            FOR_ALL_SIZES(td->size, PUT_SCALAR);
            return 0;
         }
         else {
            const int64_t length = td->wrapped
               ? ffi_array_length(c->args[slot + 2].integer)
               : td->numElems;

            if (offset + num_elems > length) {
               vhpi_error(vhpiError, &(obj->loc), "too many values (%d) for "
                          "object with %"PRIi64" elements", num_elems, length);
               return 1;
            }

            memcpy(c->args[slot].pointer + offset * td->size, ext,
                   num_elems * td->size);
            return 0;
         }
      case vhpiRelease:
         return 0;   // Specified to have no effect
      default:
         break;
      }
   }

   vhpi_error(vhpiFailure, &(obj->loc), "mode %s not supported in "
              "vhpi_put_value", vhpi_put_value_mode_str(mode));
   return 1;
}

DLLEXPORT
int vhpi_protected_call(vhpiHandleT varHdl,
                        vhpiUserFctT userFct,
                        void *userData)
{
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
   VHPI_MISSING;
}

DLLEXPORT
int vhpi_format_value(const vhpiValueT *in_value_p,
                      vhpiValueT *out_value_p)
{
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

   time_p->high = next >> 32;
   time_p->low  = next & 0xffffffff;

   return next == TIME_HIGH ? vhpiNoActivity : 0;
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
   VHPI_MISSING;
}

DLLEXPORT
int vhpi_get_foreignf_info(vhpiHandleT handle, vhpiForeignDataT *foreignDatap)
{
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
         f->data.libraryName = (char *)new_string(foreignDatap->libraryName);

         vhpi_context_t *c = vhpi_context();
         APUSH(c->foreignfs, &(f->object));

         return handle_for(&(f->object));
      }

   default:
      vhpi_error(vhpiInternal, NULL, "foreign model kind not supported");
      return NULL;
   }
}

DLLEXPORT
size_t vhpi_get_data(int32_t id, void *dataLoc, size_t numBytes)
{
   VHPI_MISSING;
}

DLLEXPORT
size_t vhpi_put_data(int32_t id, void *dataLoc, size_t numBytes)
{
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

static c_intRange *build_int_range(tree_t r, type_t parent, int dim,
                                   c_vhpiObject *obj)
{
   range_kind_t dir = RANGE_TO;
   int64_t low, high, left = 1, right = 0;
   if (folded_bounds(r, &low, &high)) {
      dir = tree_subkind(r);
      left = (dir == RANGE_TO) ? low : high;
      right = (dir == RANGE_TO) ? high : low;
   }
   else {
      ffi_dim_t *bounds = NULL;
      if (obj != NULL)
         vhpi_get_uarray(obj, NULL, &bounds);
      else if (parent != NULL && !type_is_unconstrained(parent)) {
         // The type bounds are not known statically so use the variable
         // generated by lower_type_bounds_var
         jit_t *j = vhpi_context()->jit;

         assert(type_has_ident(parent));
         ident_t id = type_ident(parent);

         jit_handle_t handle = jit_lazy_compile(j, ident_runtil(id, '.'));
         ffi_uarray_t *u = jit_get_frame_var(j, handle, id);
         assert(u != NULL);

         bounds = u->dims;
      }

      if (bounds == NULL)
         vhpi_error(vhpiInternal, tree_loc(r), "cannot get bounds for range");
      else {
         dir = ffi_array_dir(bounds[dim].length);
         left = bounds[dim].left;
         right = ffi_array_right(bounds[dim].left, bounds[dim].length);
      }
   }

   const bool null = dir == RANGE_TO ? left > right : right > left;

   c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
   init_range(&(ir->range), dir == RANGE_TO, null, true);

   ir->LeftBound  = left;
   ir->RightBound = right;

   return ir;
}

static c_intRange *build_unconstrained(void)
{
   c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
   ir->range.IsUnconstrained = vhpiTrue;
   return ir;
}

static c_typeDecl *build_arrayTypeDecl(type_t type, tree_t decl,
                                       type_t parent, c_vhpiObject *obj,
                                       int nextdim)
{
   c_arrayTypeDecl *td =
      new_object(sizeof(c_arrayTypeDecl), vhpiArrayTypeDeclK);
   init_compositeTypeDecl(&(td->composite), decl, type);

   td->NumDimensions = dimension_of(type);
   td->composite.typeDecl.wrapped = !type_const_bounds(type);

   type_t elem = type_elem(type);
   if (type_is_array(elem) && is_anonymous_subtype(elem)) {
      // Anonymous subtype may need to access parent type to
      // get non-constant bounds
      td->ElemType = build_arrayTypeDecl(elem, decl, parent, obj,
                                         nextdim + td->NumDimensions);
   }
   else {
      c_vhpiObject *tobj = &(td->composite.typeDecl.decl.object);
      td->ElemType = cached_typeDecl(elem, tobj);
   }

   td->composite.typeDecl.size = td->ElemType->size;

   vhpi_list_reserve(&td->Constraints, td->NumDimensions);

   if (type_is_unconstrained(type)) {
      for (int i = 0; i < td->NumDimensions; i++) {
         c_intRange *ir = build_unconstrained();
         vhpi_list_add(&td->Constraints, &(ir->range.object));
      }
   }
   else {
      td->composite.typeDecl.numElems = td->ElemType->numElems;

      for (int i = 0; i < td->NumDimensions; i++) {
         tree_t r = range_of(type, i);
         c_intRange *ir = build_int_range(r, parent, nextdim + i, obj);
         td->composite.typeDecl.numElems *= range_len(ir);
         vhpi_list_add(&td->Constraints, &(ir->range.object));
      }
   }

   hash_put(vhpi_context()->objcache, type, td);

   return &(td->composite.typeDecl);
}

static c_typeDecl *build_dynamicSubtype(c_typeDecl *base, void *ptr,
                                        vhpiClassKindT kind)
{
   c_subTypeDecl *td = new_object(sizeof(c_arrayTypeDecl), vhpiSubtypeDeclK);
   init_typeDecl(&(td->typeDecl), base->decl.tree, base->type);

   assert(type_is_unconstrained(base->type));
   assert(base->IsUnconstrained);

   td->typeDecl.BaseType = base;
   td->typeDecl.IsUnconstrained = false;
   td->typeDecl.IsComposite = true;

   const bool is_signal =
      kind == vhpiPortDeclK || kind == vhpiSigDeclK;

   c_arrayTypeDecl *at = is_arrayTypeDecl(&(base->decl.object));
   if (at != NULL) {
      td->typeDecl.numElems = at->ElemType->numElems;
      td->typeDecl.wrapped = true;

      ffi_dim_t *bounds =
         ptr + (is_signal ? 2 * sizeof(int64_t) : sizeof(int64_t));

      vhpi_list_reserve(&td->Constraints, at->NumDimensions);

      for (int i = 0; i < at->NumDimensions; i++) {
         const range_kind_t dir = ffi_array_dir(bounds[i].length);
         const int64_t left = bounds[i].left;
         const int64_t right =
            ffi_array_right(bounds[i].left, bounds[i].length);
         const bool null = dir == RANGE_TO ? left > right : right > left;

         c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
         init_range(&(ir->range), dir == RANGE_TO, null, true);

         ir->LeftBound  = left;
         ir->RightBound = right;

         td->typeDecl.numElems *= range_len(ir);
         vhpi_list_add(&td->Constraints, &(ir->range.object));
      }
   }

   c_recordTypeDecl *rt = is_recordTypeDecl(&(base->decl.object));
   if (rt != NULL) {
      const jit_layout_t *l =
         is_signal ? signal_layout_of(base->type) :layout_of(base->type);

      vhpiObjectListT *elems =
         expand_lazy_list(&(base->decl.object), &rt->RecordElems);

      vhpi_list_reserve(&td->Constraints, elems->count);

      for (int i = 0; i < elems->count; i++) {
         c_elemDecl *ed = is_elemDecl(elems->items[i]);
         assert(ed != NULL);

         if (ed->Type->IsUnconstrained) {
            void *fptr = ptr + l->parts[i].offset;
            c_typeDecl *sub = build_dynamicSubtype(ed->Type, fptr, kind);

            c_elemDecl *new = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
            init_elemDecl(new, ed->decl.tree, sub, rt);

            vhpi_list_add(&td->Constraints, &(new->decl.object));
         }
         else
            vhpi_list_add(&td->Constraints, &(ed->decl.object));
      }
   }

   return &(td->typeDecl);
}

static c_typeDecl *build_subTypeDecl(type_t type, tree_t where,
                                     c_vhpiObject *obj)
{
   if (type_is_array(type)) {   // TODO: should this return c_subTypeDecl?
      type_t base = type;
      while (is_anonymous_subtype(base))
         base = type_base(base);   // Anonymous subtypes have no declaration

      c_typeDecl *td = build_arrayTypeDecl(type, where, base, obj, 0);
      if (type != base)
         td->decl.Name = td->decl.CaseName = new_string(type_pp(base));

      return td;
   }

   c_subTypeDecl *td = new_object(sizeof(c_subTypeDecl), vhpiSubtypeDeclK);
   init_typeDecl(&(td->typeDecl), where, type);

   hash_put(vhpi_context()->objcache, type, td);

   td->typeDecl.BaseType = cached_typeDecl(type_base(type), NULL);
   td->typeDecl.IsAnonymous = is_anonymous_subtype(type);
   td->typeDecl.IsScalar = td->typeDecl.BaseType->IsScalar;
   td->typeDecl.IsComposite = td->typeDecl.BaseType->IsComposite;

   td->isResolved = type_has_resolution(type);

   c_recordTypeDecl *rtd =
      is_recordTypeDecl(&(td->typeDecl.BaseType->decl.object));
   if (rtd != NULL) {
      vhpiObjectListT *elems =
         expand_lazy_list(&(td->typeDecl.BaseType->decl.object),
                          &rtd->RecordElems);

      vhpi_list_reserve(&td->Constraints, elems->count);

      for (int i = 0; i < elems->count; i++) {
         c_elemDecl *ed = is_elemDecl(elems->items[i]);
         assert(ed != NULL);

         if (ed->Type->IsUnconstrained) {
            tree_t cons = type_constraint_for_field(type, ed->decl.tree);
            if (cons != NULL) {
               c_elemDecl *new = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
               init_elemDecl(new, ed->decl.tree, NULL, rtd);

               vhpi_list_add(&td->Constraints, &(new->decl.object));

               new->Type = cached_typeDecl(tree_type(cons),
                                           &(new->decl.object));

               continue;
            }
         }

         vhpi_list_add(&td->Constraints, &(ed->decl.object));
      }
   }

   if (td->typeDecl.IsScalar && type_constraints(type) > 0) {
      tree_t c = type_constraint(type, 0);
      assert(tree_subkind(c) == C_RANGE);

      vhpi_list_reserve(&td->Constraints, 1);

      tree_t r = tree_range(c, 0);
      type_t rtype = tree_type(r);
      if (type_is_real(rtype)) {
         c_floatRange *fr = build_float_range(r);
         vhpi_list_add(&td->Constraints, &(fr->range.object));
      }
      else if (type_is_physical(rtype)) {
         c_physRange *pr = build_phys_range(r);
         vhpi_list_add(&td->Constraints, &(pr->range.object));
      }
      else {
         c_intRange *ir = build_int_range(r, NULL, 0, NULL);
         vhpi_list_add(&td->Constraints, &(ir->range.object));
      }
   }

   return &(td->typeDecl);
}

static c_typeDecl *build_anonymousSubTypeDecl(type_t type, c_vhpiObject *obj)
{
   assert(is_anonymous_subtype(type));

   c_abstractDecl *decl = is_abstractDecl(obj);
   assert(decl != NULL);

   c_typeDecl *td = build_subTypeDecl(type, decl->tree, obj);
   td->IsAnonymous = true;

   return td;
}

static c_typeDecl *build_typeDecl(tree_t decl, c_abstractRegion *region)
{
   type_t type = tree_type(decl);

   switch (type_kind(type)) {
   case T_INTEGER:
      {
         c_intTypeDecl *td =
            new_object(sizeof(c_intTypeDecl), vhpiIntTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&(region->decls.list), tobj);
         hash_put(vhpi_context()->objcache, type, td);

         c_intRange *ir = build_int_range(range_of(type, 0), NULL, 0, NULL);
         td->constraint = &(ir->range);

         return &(td->scalar.typeDecl);
      }

   case T_ENUM:
      {
         c_enumTypeDecl *td =
            new_object(sizeof(c_enumTypeDecl), vhpiEnumTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&region->decls.list, tobj);
         hash_put(vhpi_context()->objcache, type, td);

         td->EnumLiterals.fn = vhpi_lazy_enum_literals;

         return &(td->scalar.typeDecl);
      }

   case T_PHYSICAL:
      {
         c_physTypeDecl *td =
            new_object(sizeof(c_physTypeDecl), vhpiPhysTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&region->decls.list, tobj);
         hash_put(vhpi_context()->objcache, type, td);

         td->constraint = &(build_phys_range(range_of(type, 0))->range);
         return &(td->scalar.typeDecl);
      }

   case T_REAL:
      {
         c_floatTypeDecl *td =
            new_object(sizeof(c_floatTypeDecl), vhpiFloatTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);

         c_vhpiObject *tobj = &(td->scalar.typeDecl.decl.object);
         vhpi_list_add(&region->decls.list, tobj);
         hash_put(vhpi_context()->objcache, type, td);

         td->constraint = &(build_float_range(range_of(type, 0))->range);
         return &(td->scalar.typeDecl);
      }

   case T_ARRAY:
      {
         c_arrayTypeDecl *td =
            new_object(sizeof(c_arrayTypeDecl), vhpiArrayTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl, type);

         c_vhpiObject *tobj = &(td->composite.typeDecl.decl.object);
         vhpi_list_add(&region->decls.list, tobj);
         hash_put(vhpi_context()->objcache, type, td);

         td->NumDimensions = dimension_of(type);
         td->composite.typeDecl.wrapped = true;
         td->ElemType = cached_typeDecl(type_elem(type), tobj);
         td->composite.typeDecl.size = td->ElemType->size;

         vhpi_list_reserve(&td->Constraints, td->NumDimensions);

         for (int i = 0; i < td->NumDimensions; i++) {
            c_intRange *ir = build_unconstrained();
            vhpi_list_add(&td->Constraints, &(ir->range.object));
         }

         return &(td->composite.typeDecl);
      }

   case T_RECORD:
      {
         c_recordTypeDecl *td =
            new_object(sizeof(c_recordTypeDecl), vhpiRecordTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl, type);

         c_vhpiObject *tobj = &(td->composite.typeDecl.decl.object);
         vhpi_list_add(&region->decls.list, tobj);
         hash_put(vhpi_context()->objcache, type, td);

         td->RecordElems.fn = vhpi_lazy_fields;

         return &(td->composite.typeDecl);
      }

   case T_SUBTYPE:
      {
         assert(!is_anonymous_subtype(type));

         c_typeDecl *td = build_subTypeDecl(type, decl, NULL);

         vhpi_list_add(&region->decls.list, &(td->decl.object));
         assert(hash_get(vhpi_context()->objcache, type) == td);

         return td;
      }

   default:
      fatal_trace("cannot build VHPI typeDecl for %s %s",
                  type_kind_str(type_kind(type)), type_pp(type));
   }
}

static c_typeDecl *cached_typeDecl(type_t type, c_vhpiObject *obj)
{
   hash_t *cache = vhpi_context()->objcache;
   c_typeDecl *d = hash_get(cache, type);
   if (d != NULL)
      return d;
   else if (is_anonymous_subtype(type)) {
      d = build_anonymousSubTypeDecl(type, obj);
      assert(hash_get(cache, type) == d);
      return d;
   }
   else {
      c_designUnit *du = cached_designUnit(type_container(type));
      expand_lazy_list(&(du->region.object), &(du->region.decls));

      if ((d = hash_get(cache, type)) == NULL)
         fatal_trace("cannot find cached type for %s", type_pp(type));

      return d;
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

         entity->designUnit.region.decls.fn = vhpi_lazy_decls;

         return &(entity->designUnit);
      }

   case T_ARCH:
      {
         c_designUnit *primary = cached_designUnit(tree_primary(t));

         c_secondaryUnit *secondary =
            new_object(sizeof(c_secondaryUnit), vhpiArchBodyK);
         init_secondaryUnit(secondary, t, primary);

         secondary->designUnit.region.decls.fn = vhpi_lazy_decls;

         return &(secondary->designUnit);
      }

   case T_PACKAGE:
      {
         c_packDecl *pack = new_object(sizeof(c_packDecl), vhpiPackDeclK);
         init_packDecl(pack, t);

         pack->designUnit.region.decls.fn = vhpi_lazy_decls;

         return &(pack->designUnit);
      }

   default:
      fatal_trace("unsupported tree kind %s in build_designUnit",
                  tree_kind_str(tree_kind(t)));
   }
}

static c_designUnit *cached_designUnit(tree_t t)
{
   assert(is_design_unit(t));

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

   g->IsLocal = (tree_kind(region->tree) == T_COMPONENT);
   g->IsVital = false;
   g->Mode = mode_map[tree_subkind(generic)];

   vhpi_list_add(&region->decls.list, &(g->interface.objDecl.decl.object));
}

static c_constParamDecl *build_constParamDecl(tree_t param, int pos)
{
   c_constParamDecl *p = new_object(sizeof(c_constParamDecl),
                                    vhpiConstParamDeclK);
   init_interfaceDecl(&(p->interface), param, pos, NULL);

   p->Mode = mode_map[tree_subkind(param)];
   return p;
}

static c_varParamDecl *build_varParamDecl(tree_t param, int pos)
{
   c_varParamDecl *p = new_object(sizeof(c_varParamDecl), vhpiVarParamDeclK);
   init_interfaceDecl(&(p->interface), param, pos, NULL);

   p->Mode = mode_map[tree_subkind(param)];
   return p;
}

static void build_portDecl(tree_t port, int pos,
                           c_abstractRegion *region)
{
   c_portDecl *p = new_object(sizeof(c_portDecl), vhpiPortDeclK);
   init_interfaceDecl(&(p->interface), port, pos, region);

   p->Mode = mode_map[tree_subkind(port)];

   vhpi_list_add(&region->decls.list, &(p->interface.objDecl.decl.object));
}

static void build_signalDecl(tree_t decl, c_abstractRegion *region)
{
   c_sigDecl *s = new_object(sizeof(c_sigDecl), vhpiSigDeclK);
   init_objDecl(&(s->objDecl), decl, region);

   vhpi_list_add(&region->decls.list, &(s->objDecl.decl.object));
}

static c_constDecl *build_constDecl(tree_t decl, c_abstractRegion *region)
{
   c_constDecl *cd = new_object(sizeof(c_constDecl), vhpiConstDeclK);
   init_objDecl(&(cd->objDecl), decl, region);

   cd->IsDeferred = !tree_has_value(decl);

   vhpi_list_add(&region->decls.list, &(cd->objDecl.decl.object));
   return cd;
}

static void build_paramDecls(tree_t decl, int first, c_subpDecl *subp)
{
   const int nparams = tree_ports(decl);
   vhpi_list_reserve(&subp->Params, nparams);

   for (int i = 0, slot = first; i < nparams; i++) {
      tree_t p = tree_port(decl, i);
      c_typeDecl *td = NULL;
      switch (tree_class(p)) {
      case C_CONSTANT:
         {
            c_constParamDecl *cpd = build_constParamDecl(p, i);
            cpd->interface.argslot = slot;
            vhpi_list_add(&subp->Params, &(cpd->interface.objDecl.decl.object));
            td = cpd->interface.objDecl.Type;
         }
         break;
      case C_VARIABLE:
         {
            c_varParamDecl *vpd = build_varParamDecl(p, i);
            vpd->interface.argslot = slot;
            vhpi_list_add(&subp->Params, &(vpd->interface.objDecl.decl.object));
            td = vpd->interface.objDecl.Type;
         }
         break;
      default:
         fatal_at(tree_loc(p), "unsupported parameter class");
      }

      slot += td->wrapped ? 1 + 2*dimension_of(td->type) : 1;
   }
}

static c_funcDecl *build_funcDecl(tree_t decl)
{
   c_funcDecl *f = new_object(sizeof(c_funcDecl), vhpiFuncDeclK);
   init_subpDecl(&(f->subpDecl), decl);

   build_paramDecls(decl, 1, &(f->subpDecl));

   f->IsPure = !(tree_flags(decl) & TREE_F_IMPURE);
   f->ReturnType = cached_typeDecl(type_result(tree_type(decl)), NULL);

   return f;
}

static c_procDecl *build_procDecl(tree_t decl)
{
   c_procDecl *f = new_object(sizeof(c_funcDecl), vhpiProcDeclK);
   init_subpDecl(&(f->subpDecl), decl);

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

   vhpi_list_add(&region->stmts.list, &(bs->region.object));

   return &(bs->region);
}

static c_abstractRegion *build_compInstStmt(tree_t t, tree_t inst,
                                            c_abstractRegion *region)
{
   assert(tree_kind(t) == T_BLOCK);

   tree_t inner = t;
   c_designUnit *du;
   switch (tree_kind(inst)) {
   case T_ARCH:
      du = cached_designUnit(inst);
      break;
   case T_COMPONENT:
      {
         assert(tree_stmts(t) == 1);

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
   c->designInstUnit.region.tree = inner;

   vhpi_list_add(&region->stmts.list, &(c->designInstUnit.region.object));

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

   vhpi_list_reserve(&(g->region.decls.list), tree_decls(t) + 1);

   tree_t g0 = tree_generic(t, 0);
   g->ParamDecl = build_constDecl(g0, &(g->region));

   vhpi_list_add(&region->stmts.list, &(g->region.object));

   return &(g->region);
}

static void build_packInst(tree_t inst, c_abstractRegion *region)
{
   c_designUnit *du = cached_designUnit(tree_ref(inst));

   c_packInst *pi = new_object(sizeof(c_packInst), vhpiPackInstK);
   init_designInstUnit(&(pi->designInstUnit), region, inst, du);
   pi->designInstUnit.region.decls.fn = vhpi_lazy_decls;

   // Must expand eagerly otherwise cached_typeDecl cannot find types
   // declared in this package
   expand_lazy_list(&(pi->designInstUnit.region.object),
                    &(pi->designInstUnit.region.decls));
}

static void vhpi_lazy_selected_names(c_vhpiObject *obj)
{
   c_typeDecl *type = NULL;
   vhpiObjectListT *list = NULL;

   c_name *n = is_name(obj);
   if (n != NULL) {
      list = &(n->SelectedNames.list);
      type = n->expr.Type;
   }

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL) {
      list = &(od->SelectedNames.list);
      type = od->Type;
   }

   assert(list != NULL && type != NULL);
   assert(list->count == 0);

   c_subTypeDecl *std = is_subTypeDecl(&(type->decl.object));
   if (std != NULL) {
      vhpi_list_reserve(list, std->Constraints.count);

      for (int i = 0; i < std->Constraints.count; i++) {
         c_elemDecl *ed = is_elemDecl(std->Constraints.items[i]);
         assert(ed != NULL);

         c_selectedName *sn =
            new_object(sizeof(c_selectedName), vhpiSelectedNameK);
         init_selectedName(sn, obj, ed);

         vhpi_list_add(list, &(sn->prefixedName.name.expr.object));
      }
   }

   c_recordTypeDecl *rtd = is_recordTypeDecl(&(type->decl.object));
   if (rtd != NULL) {
      vhpiObjectListT *elems =
         expand_lazy_list(&(type->decl.object), &(rtd->RecordElems));

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
   c_typeDecl *type = NULL;
   vhpiObjectListT *list = NULL;

   c_name *n = is_name(obj);
   if (n != NULL) {
      list = &(n->IndexedNames.list);
      type = n->expr.Type;
   }

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL) {
      list = &(od->IndexedNames.list);
      type = od->Type;
   }

   assert(list != NULL && type != NULL);
   assert(list->count == 0);

   vhpiObjectListT *Constraints;
   c_subTypeDecl *std = is_subTypeDecl(&(type->decl.object));
   c_arrayTypeDecl *atd = is_arrayTypeDecl(&(type->decl.object));
   if (std != NULL) {
      Constraints = &(std->Constraints);
      atd = is_arrayTypeDecl(&(type->BaseType->decl.object));
      if (atd == NULL)
         return;
   }
   else if (atd != NULL)
      Constraints = &(atd->Constraints);
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
      init_indexedName(in, atd->ElemType, obj, Constraints, indices);
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

static void vhpi_lazy_fields(c_vhpiObject *obj)
{
   c_recordTypeDecl *td = is_recordTypeDecl(obj);
   assert(td != NULL);

   const int nfields = type_fields(td->composite.typeDecl.type);
   vhpi_list_reserve(&td->RecordElems.list, nfields);

   for (int i = 0; i < nfields; i++) {
      tree_t f = type_field(td->composite.typeDecl.type, i);
      type_t ft = tree_type(f);

      c_elemDecl *ed = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
      init_elemDecl(ed, f, NULL, td);

      vhpi_list_add(&td->RecordElems.list, &(ed->decl.object));

      ed->Type = cached_typeDecl(ft, &(ed->decl.object));
      td->composite.typeDecl.numElems += ed->Type->numElems;
   }
}

static void vhpi_lazy_stmts(c_vhpiObject *obj)
{
   c_abstractRegion *r = is_abstractRegion(obj);
   assert(r != NULL);

   const int nstmts = tree_stmts(r->tree);
   vhpi_list_reserve(&r->stmts.list, nstmts);

   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(r->tree, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         {
            tree_t h = tree_decl(s, 0);
            assert(tree_kind(h) == T_HIER);

            c_abstractRegion *sub = NULL;
            switch (tree_subkind(h)) {
            case T_BLOCK:
               sub = build_blockStmt(s, r);
               sub->stmts.fn = vhpi_lazy_stmts;
               sub->decls.fn = vhpi_lazy_decls;
               break;
            case T_ARCH:
            case T_COMPONENT:
               sub = build_compInstStmt(s, tree_ref(h), r);
               sub->stmts.fn = vhpi_lazy_stmts;
               sub->decls.fn = vhpi_lazy_decls;
               break;
            case T_FOR_GENERATE:
               sub = build_forGenerate(s, r);
               sub->stmts.fn = vhpi_lazy_stmts;
               sub->decls.fn = vhpi_lazy_decls;
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

   int ndecls = tree_decls(r->tree), nports = 0, ngenerics = 0;

   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiCompInstStmtK:
   case vhpiEntityDeclK:
      nports = tree_ports(r->tree);
      // Fall-through
   case vhpiPackDeclK:
      ngenerics = tree_generics(r->tree);
      break;
   default:
      break;
   }

   vhpi_list_reserve(&r->decls.list, ndecls + nports + ngenerics);

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(r->tree, i);
      build_portDecl(p, i, r);
   }

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(r->tree, i);
      build_genericDecl(g, i, r);
   }

   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(r->tree, i);
      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
         build_signalDecl(d, r);
         break;
      case T_CONST_DECL:
         build_constDecl(d, r);
         break;
      case T_TYPE_DECL:
      case T_SUBTYPE_DECL:
         build_typeDecl(d, r);
         break;
      case T_PACK_INST:
         build_packInst(d, r);
         break;
      default:
         break;
      }
   }
}

static void vhpi_build_deps_cb(ident_t name, void *ctx)
{
   hset_t *visited = ctx;

   if (hset_contains(visited, name))
      return;

   hset_insert(visited, name);

   tree_t unit = lib_get_qualified(name);
   if (unit == NULL)
      return;
   else if (tree_kind(unit) == T_PACKAGE) {
      c_designUnit *pack = cached_designUnit(unit);
      assert(pack->region.object.kind == vhpiPackDeclK);

      c_packInst *pi = new_object(sizeof(c_packInst), vhpiPackInstK);
      init_designInstUnit(&(pi->designInstUnit), NULL, unit, pack);

      pi->designInstUnit.region.decls.fn = vhpi_lazy_decls;

      vhpi_context_t *c = vhpi_context();
      assert(hash_get(c->objcache, unit) == pack);
      hash_put(c->objcache, unit, pi);

      APUSH(c->packages, &(pi->designInstUnit.region.object));
   }

   tree_walk_deps(unit, vhpi_build_deps_cb, visited);
}

static void vhpi_run_callbacks(int32_t reason, int32_t rep)
{
   vhpi_context_t *c = vhpi_context();

   const int orig_count = c->callbacks.count;
   int wptr = 0;
   for (int i = 0; i < orig_count; i++) {
      vhpiHandleT handle = c->callbacks.items[i];

      handle_slot_t *slot = decode_handle(c, handle);
      if (slot == NULL)
         continue;

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

   c_abstractRegion *region = &(c->root->designInstUnit.region);
   region->stmts.fn = vhpi_lazy_stmts;
   region->decls.fn = vhpi_lazy_decls;

   vhpi_run_callbacks(vhpiCbEndOfInitialization, 0);
}

static void vhpi_phase_cb(rt_model_t *m, void *arg)
{
   const int32_t reason = (intptr_t)arg;

   int32_t rep = 0;
   switch (reason) {
   case vhpiCbEndOfProcesses:      rep = vhpiCbRepEndOfProcesses; break;
   case vhpiCbLastKnownDeltaCycle: rep = vhpiCbRepLastKnownDeltaCycle; break;
   case vhpiCbNextTimeStep:        rep = vhpiCbRepNextTimeStep; break;
   case vhpiCbEndOfTimeStep:       rep = vhpiCbRepEndOfTimeStep; break;
   case vhpiCbStartOfNextCycle:    rep = vhpiCbRepStartOfNextCycle; break;
   }

   vhpi_run_callbacks(reason, rep);

   switch (reason) {
   case vhpiCbEndOfProcesses:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbNextTimeStep:
   case vhpiCbEndOfTimeStep:
   case vhpiCbStartOfNextCycle:
      model_set_global_cb(m, vhpi_get_rt_event(reason), vhpi_phase_cb,
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
   c->tool     = new_object(sizeof(c_tool), vhpiToolK);

   return c;
}

void vhpi_context_initialise(vhpi_context_t *c, tree_t top, rt_model_t *model,
                             jit_t *jit, int argc, char **argv)
{
   assert(c->model == NULL);
   assert(c->top == NULL);
   assert(c->jit == NULL);

   c->model = model;
   c->top   = top;
   c->jit   = jit;

   vhpi_list_reserve(&c->tool->argv, argc);

   for (int i = 0; i < argc; i++) {
      c_argv *arg = new_object(sizeof(c_argv), vhpiArgvK);
      arg->StrVal = new_string(argv[i]);
      vhpi_list_add(&c->tool->argv, &(arg->object));
   }

   hset_t *visited = hset_new(64);
   tree_walk_deps(c->top, vhpi_build_deps_cb, visited);
   hset_free(visited);

   model_set_global_cb(model, RT_END_OF_INITIALISATION, vhpi_initialise_cb, c);

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
      model_set_global_cb(model, vhpi_get_rt_event(reasons[i]),
                          vhpi_phase_cb, (void *)(uintptr_t)reasons[i]);
}

static void vhpi_check_leaks(vhpi_context_t *c)
{
   int nactive = 0;
   for (int i = 0; i < c->num_handles; i++) {
      if (c->handles[i].obj != NULL)
         nactive++;
   }

   if (nactive > 0) {
      diag_t *d = diag_new(DIAG_WARN, NULL);
      diag_printf(d, "VHPI program exited with %d active handles",
                  nactive);

      for (int i = 0; i < c->num_handles; i++) {
         handle_slot_t *slot = &(c->handles[i]);
         if (slot->obj != NULL)
            diag_printf(d, "\n%s with %d references",
                        handle_pp(slot->obj->handle), slot->refcount);
      }

      diag_emit(d);
   }
}

void vhpi_context_free(vhpi_context_t *c)
{
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

   if (opt_get_int(OPT_VHPI_DEBUG))
      vhpi_check_leaks(c);

   assert(c == global_context);
   global_context = NULL;

   if (c->strtab != NULL)
      shash_free(c->strtab);

#ifdef DEBUG
   size_t alloc, npages;
   pool_stats(c->pool, &alloc, &npages);
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
                              tree_t where)
{
   vhpi_context_t *c = vhpi_context();
   for (int i = 0; i < c->foreignfs.count; i++) {
      c_foreignf *f = cast_foreignf(c->foreignfs.items[i]);
      if (strcmp(f->data.libraryName, obj_lib))
         continue;
      else if (strcmp(f->data.modelName, model))
         continue;

      assert(tree_kind(where) == T_ATTR_SPEC);
      tree_t sub = tree_ref(where);

      if (f->decl != NULL && f->decl->tree == sub)
         return f->handle;
      else if (f->decl != NULL) {
         // Foreign functions may be registered during elaboration after
         // which the object may be moved
         tree_t reloc = tree_from_locus(f->decl->module, f->decl->offset, NULL);
         if (reloc == sub) {
            tree_locus(sub, &f->decl->module, &f->decl->offset);
            f->decl->tree = sub;
            return f->handle;
         }
         else
            jit_msg(NULL, DIAG_FATAL, "foreign subprogram %s/%s already bound "
                    "to %s", obj_lib, model, type_pp(tree_type(f->decl->tree)));
      }

      switch (tree_kind(sub)) {
      case T_FUNC_DECL:
      case T_FUNC_BODY:
         f->decl = &(build_funcDecl(sub)->subpDecl);
         break;
      case T_PROC_DECL:
      case T_PROC_BODY:
         f->decl = &(build_procDecl(sub)->subpDecl);
         break;
      default:
         jit_msg(NULL, DIAG_FATAL, "unsupported foreign subprogram");
      }

      return (f->handle = handle_for(&(f->object)));
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

   vhpiHandleT subp = handle_for(&(f->decl->object));
   vhpiCbDataT data = {
      .obj = subp
   };
   (*f->data.execf)(&data);

   drop_handle(c, subp);
   c->args = NULL;
   c->tlab = NULL;

   if (f->decl->object.kind == vhpiFuncDeclK && args[0].pointer == orig_p0)
      jit_msg(NULL, DIAG_FATAL, "foreign function %s did not return a value",
              type_pp(tree_type(f->decl->tree)));
}
