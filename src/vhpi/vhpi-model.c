//
//  Copyright (C) 2014-2022  Nick Gasson
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
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

typedef vhpiCharT      *vhpiStringT;
typedef vhpiSmallEnumT  vhpiBooleanT;

typedef struct {
   vhpiClassKindT kind;
   loc_t          loc;
} c_vhpiObject;

typedef A(c_vhpiObject *) vhpiObjectListT;

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

typedef void (*lazy_region_fn_t)(c_abstractRegion *);

typedef struct tag_abstractRegion {
   c_vhpiObject      object;
   tree_t            tree;
   rt_scope_t       *scope;
   lazy_region_fn_t  lazyfn;
   vhpiObjectListT   decls;
   vhpiObjectListT   stmts;
   vhpiObjectListT   InternalRegions;
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
   c_range  range;
   vhpiIntT LeftBound;
   vhpiIntT RightBound;
} c_intRange;

DEF_CLASS(intRange, vhpiIntRangeK, range.object);

typedef struct tag_typeDecl c_typeDecl;

typedef struct tag_typeDecl {
   c_abstractDecl  decl;
   type_t          type;
   c_typeDecl     *BaseType;
   vhpiFormatT     format;
   const char     *map_str;
   vhpiIntT        numElems;
   vhpiBooleanT    IsAnonymous;
   vhpiBooleanT    IsComposite;
   vhpiBooleanT    IsScalar;
   vhpiBooleanT    IsUnconstrained;
   bool            homogeneous;
   bool            wrapped;
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
} c_intTypeDecl;

typedef struct {
   c_scalarTypeDecl scalar;
   vhpiObjectListT  EnumLiterals;
} c_enumTypeDecl;

DEF_CLASS(enumTypeDecl, vhpiEnumTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_scalarTypeDecl scalar;
   c_range         *constraint;
} c_physTypeDecl;

DEF_CLASS(physTypeDecl, vhpiPhysTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_scalarTypeDecl scalar;
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
   vhpiObjectListT     RecordElems;
} c_recordTypeDecl;

DEF_CLASS(recordTypeDecl, vhpiRecordTypeDeclK, composite.typeDecl.decl.object);

typedef struct {
   c_abstractDecl  decl;
   c_typeDecl     *Type;
   vhpiIntT        Position;
} c_elemDecl;

DEF_CLASS(elemDecl, vhpiElemDeclK, decl.object);

typedef struct {
   c_abstractDecl   decl;
   vhpiObjectListT  IndexedNames;
   vhpiObjectListT  SelectedNames;
   c_typeDecl      *Type;
   vhpiIntT         Access;
   vhpiStaticnessT  Staticness;
   vhpiBooleanT     IsDynamic;
   bool             IndexedNames_valid;
   bool             SelectedNames_valid;
   rt_signal_t     *signal;
   rt_scope_t      *scope;
   ident_t          name;
} c_objDecl;

typedef struct {
   c_objDecl    objDecl;
   vhpiSigKindT SigKind;
   vhpiBooleanT IsGuarded;
} c_sigDecl;

DEF_CLASS(sigDecl, vhpiSigDeclK, objDecl.decl.object);

typedef struct {
   c_objDecl objDecl;
   vhpiIntT  Position;
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
   c_expr          expr;
   vhpiObjectListT IndexedNames;
   vhpiObjectListT SelectedNames;
   vhpiStringT     FullCaseName;
   vhpiStringT     CaseName;
   vhpiStringT     FullName;
   vhpiStringT     Name;
   vhpiStringT     DefName;
   vhpiAccessT     Access;
   bool            IndexedNames_valid;
   bool            SelectedNames_valid;
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
   c_vhpiObject    object;
   vhpiStringT     CaseName;
   vhpiStringT     Name;
   vhpiStringT     UnitName;
   vhpiStringT     FileName;
   vhpiIntT        LineOffset;
   vhpiIntT        LineNo;
   vhpiObjectListT Decls;
   vhpiObjectListT DepUnits;
} c_designUnit;

typedef struct {
   c_designUnit    designUnit;
   c_designUnit   *PrimaryUnit;
} c_secondaryUnit;

DEF_CLASS(secondaryUnit, vhpiArchBodyK, designUnit.object);

typedef struct {
   c_designUnit    designUnit;
   vhpiObjectListT ports;
} c_entityDecl;

typedef struct {
   c_abstractRegion  region;
   c_designUnit     *DesignUnit;
} c_designInstUnit;

typedef struct {
   c_designInstUnit designInstUnit;
} c_rootInst;

DEF_CLASS(rootInst, vhpiRootInstK, designInstUnit.region.object);

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

typedef enum {
   CB_INACTIVE,
   CB_ACTIVE,
   CB_FREE_LATER
} callback_status_t;

typedef struct {
   c_vhpiObject      object;
   vhpiStateT        State;
   vhpiEnumT         Reason;
   vhpiCbDataT       data;
   union {
      uint64_t       when;
      rt_watch_t    *w;
   };
   callback_status_t status;
} c_callback;

DEF_CLASS(callback, vhpiCallbackK, object);

typedef struct {
   c_vhpiObject     object;
   vhpiObjectListT *list;
   c_vhpiObject    *single;
   uint32_t         pos;
   vhpiClassKindT   filter;
} c_iterator;

DEF_CLASS(iterator, vhpiIteratorK, object);

typedef struct _vhpi_context {
   c_tool     *tool;
   c_rootInst *root;
   shash_t    *strtab;
   rt_model_t *model;
   hash_t     *typecache;
   tree_t      top;
   jit_t      *jit;
} vhpi_context_t;

static c_typeDecl *cached_typeDecl(type_t type, c_vhpiObject *obj);
static c_typeDecl *build_dynamicSubtype(c_typeDecl *base, void *ptr,
                                        vhpiClassKindT kind);
static void vhpi_lazy_block(c_abstractRegion *r);
static void *vhpi_get_value_ptr(c_vhpiObject *obj);
static c_typeDecl *vhpi_get_type(c_vhpiObject *obj);
static vhpiClassKindT vhpi_get_prefix_kind(c_vhpiObject *obj);

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

static vhpiHandleT handle_for(c_vhpiObject *obj)
{
   return (vhpiHandleT)obj;
}

static c_vhpiObject *from_handle(vhpiHandleT handle)
{
   if (handle == NULL)
      vhpi_error(vhpiError, NULL, "invalid handle");

   return (c_vhpiObject *)handle;
}

static c_abstractRegion *is_abstractRegion(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiBlockStmtK:
   case vhpiCompInstStmtK:
   case vhpiForGenerateK:
      return container_of(obj, c_abstractRegion, object);
   default:
      return NULL;
   }
}

static c_abstractRegion *cast_abstractRegion(c_vhpiObject *obj)
{
   c_abstractRegion *r = is_abstractRegion(obj);
   if (r == NULL)
      vhpi_error(vhpiError, &(obj->loc), "class kind %s is not a region",
                 vhpi_class_str(obj->kind));
   return r;
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
      return container_of(obj, c_designUnit, object);
   default:
      return NULL;
   }
}

static c_designInstUnit *cast_designInstUnit(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
   case vhpiCompInstStmtK:
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

   c_vhpiObject *obj = from_handle(handle);
   tb_cat(tb, vhpi_class_str(obj->kind));

   c_abstractDecl *decl = is_abstractDecl(obj);
   if (decl != NULL)
      tb_printf(tb, " Name=%s", decl->Name);

   c_name *n = is_name(obj);
   if (n != NULL)
      tb_printf(tb, " Name=%s", n->Name);

   c_iterator *it = is_iterator(obj);
   if (it != NULL)
      tb_printf(tb, " pos=%d/%d", it->pos, it->list->count);

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

   c_vhpiObject *obj = xcalloc(size);
   obj->kind = class;
   obj->loc = LOC_INVALID;

   return obj;
}

static vhpiCharT *new_string(const char *s)
{
   shash_t *strtab = vhpi_context()->strtab;
   vhpiCharT *p = shash_get(strtab, s);
   if (p == NULL)
      shash_put(strtab, s, (p = (vhpiCharT *)xstrdup(s)));
   return p;
}

static c_tool *build_tool(int argc, char **argv)
{
   c_tool *t = new_object(sizeof(c_tool), vhpiToolK);

   for (int i = 0; i < argc; i++) {
      c_argv *arg = new_object(sizeof(c_argv), vhpiArgvK);
      arg->StrVal = new_string(argv[i]);
      APUSH(t->argv, &(arg->object));
   }

   return t;
}

static void init_abstractRegion(c_abstractRegion *r, tree_t t)
{
   const loc_t *loc = tree_loc(t);
   r->object.loc = *loc;

   r->LineNo     = loc->first_line;
   r->LineOffset = loc->line_delta;

   r->Name = r->CaseName = new_string(istr(tree_ident(t)));
   char *full LOCAL = xasprintf(":%s", r->Name);
   r->FullName = r->FullCaseName = new_string(full);

   r->tree   = t;
   r->handle = JIT_HANDLE_INVALID;
}

static void init_designInstUnit(c_designInstUnit *iu, tree_t t, c_designUnit *u)
{
   init_abstractRegion(&(iu->region), t);

   iu->DesignUnit = u;
}

static void init_abstractDecl(c_abstractDecl *d, tree_t t, c_abstractRegion *r)
{
   const loc_t *loc = tree_loc(t);
   d->object.loc = *loc;

   d->LineNo     = loc->first_line;
   d->LineOffset = loc->line_delta;

   d->Name = d->CaseName = new_string(istr(tree_ident(t)));
   if (r) {
      char *full LOCAL = xasprintf("%s:%s", r->FullName, d->Name);
      d->FullName = d->FullCaseName = new_string(full);
   }

   d->ImmRegion = r;

   d->type = tree_type(t);
   d->tree = t;
}

static void init_objDecl(c_objDecl *d, tree_t t, c_abstractRegion *ImmRegion)
{
   init_abstractDecl(&(d->decl), t, ImmRegion);

   type_t type = tree_type(t);

   d->name = tree_ident(t);
   d->Type = cached_typeDecl(type, &(d->decl.object));

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
}

static void init_interfaceDecl(c_interfaceDecl *d, tree_t t,
                               int Position, c_abstractRegion *ImmRegion)
{
   init_objDecl(&(d->objDecl), t, ImmRegion);
   d->Position = Position;
}

static void init_elemDecl(c_elemDecl *ed, tree_t t, c_typeDecl *Type,
                          c_abstractRegion *ImmRegion)
{
   init_abstractDecl(&(ed->decl), t, ImmRegion);
   ed->Type = Type;
   ed->Position = tree_pos(t);
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

static void init_range(c_range *r, vhpiBooleanT IsUp)
{
   r->IsUp = IsUp;
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
}

static void init_prefixedName(c_prefixedName *pn, c_typeDecl *Type,
                              c_vhpiObject *prefix, const char *suffix)
{
   vhpiStringT Name = NULL, FullName = NULL;

   c_name *name = is_name(prefix);
   if (name != NULL) {
      Name = name->Name;
      FullName = name->FullName;
   }

   vhpiStaticnessT Staticness = vhpiDynamic;
   c_objDecl *obj = is_objDecl(prefix);
   if (obj != NULL) {
      Name = obj->decl.Name;
      FullName = obj->decl.FullName;
      Staticness = obj->Staticness;
   }

   Name = (vhpiStringT)xasprintf("%s%s", Name, suffix);
   FullName = (vhpiStringT)xasprintf("%s%s", FullName, suffix);

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

static void vhpi_build_indexedNames(vhpiObjectListT *IndexedNames,
                                    c_vhpiObject *prefix, c_typeDecl *Type)
{
   assert(Type != NULL);

   vhpiObjectListT *Constraints;
   c_subTypeDecl *std = is_subTypeDecl(&(Type->decl.object));
   c_arrayTypeDecl *atd = is_arrayTypeDecl(&(Type->decl.object));
   if (std != NULL) {
      Constraints = &(std->Constraints);
      atd = is_arrayTypeDecl(&(Type->BaseType->decl.object));
      if (atd == NULL)
         return;
   }
   else if (atd != NULL)
      Constraints = &(atd->Constraints);
   else
      return;

   if (Constraints->count == 0)
      return;

   vhpiIntT *lens LOCAL = xmalloc_array(Constraints->count, sizeof(vhpiIntT));
   for (int i = 0; i < Constraints->count; i++) {
      c_intRange *ir = is_intRange(Constraints->items[i]);
      assert(ir != NULL);

      if ((lens[i] = range_len(ir)) == 0)
         return;
   }

   int pos = 0;
   vhpiIntT *indices LOCAL = xcalloc_array(Constraints->count, sizeof(vhpiIntT));
   do {
      c_indexedName *in = new_object(sizeof(c_indexedName), vhpiIndexedNameK);
      init_indexedName(in, atd->ElemType, prefix, Constraints, indices);
      APUSH(*IndexedNames, &(in->prefixedName.name.expr.object));

      for (pos = Constraints->count - 1; pos >= 0; pos--) {
         indices[pos]++;
         if (indices[pos] >= lens[pos])
            indices[pos] = 0;
         else
            break;
      }
   } while (pos > 0 || indices[0] != 0);
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

static void vhpi_build_selectedNames(vhpiObjectListT *SelectedNames,
                                     c_vhpiObject *prefix, c_typeDecl *Type)
{
   assert(Type != NULL);

   c_subTypeDecl *std = is_subTypeDecl(&(Type->decl.object));
   if (std != NULL) {
      for (int i = 0; i < std->Constraints.count; i++) {
         c_elemDecl *ed = is_elemDecl(std->Constraints.items[i]);
         assert(ed != NULL);

         c_selectedName *sn =
            new_object(sizeof(c_selectedName), vhpiSelectedNameK);
         init_selectedName(sn, prefix, ed);
         APUSH(*SelectedNames, &(sn->prefixedName.name.expr.object));
      }
   }

   c_recordTypeDecl *rtd = is_recordTypeDecl(&(Type->decl.object));
   if (rtd != NULL) {
      for (int i = 0; i < rtd->RecordElems.count; i++) {
         c_elemDecl *ed = is_elemDecl(rtd->RecordElems.items[i]);
         assert(ed != NULL);

         c_selectedName *sn =
            new_object(sizeof(c_selectedName), vhpiSelectedNameK);
         init_selectedName(sn, prefix, ed);
         APUSH(*SelectedNames, &(sn->prefixedName.name.expr.object));
      }
   }
}

static void init_designUnit(c_designUnit *u, tree_t t)
{
   const loc_t *loc = tree_loc(t);
   u->object.loc = *loc;

   u->LineNo     = loc->first_line;
   u->LineOffset = loc->line_delta;

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

static void expand_lazy_region(c_abstractRegion *r)
{
   if (r->lazyfn == NULL)
      return;

   (*r->lazyfn)(r);
   r->lazyfn = NULL;
}

static bool init_iterator(c_iterator *it, vhpiOneToManyT type, c_vhpiObject *obj)
{
   it->filter = vhpiUndefined;

   c_abstractRegion *region = is_abstractRegion(obj);
   if (region != NULL) {
      expand_lazy_region(region);

      switch (type) {
      case vhpiDecls:
         it->list = &(region->decls);
         return true;
      case vhpiInternalRegions:
         it->list = &(region->InternalRegions);
         return true;
      case vhpiConstDecls:
         it->filter = vhpiConstDeclK;
         it->list = &(region->decls);
         return true;
      case vhpiVarDecls:
         it->filter = vhpiVarDeclK;
         it->list = &(region->decls);
         return true;
      case vhpiSigDecls:
         it->filter = vhpiSigDeclK;
         it->list = &(region->decls);
         return true;
      case vhpiGenericDecls:
         it->filter = vhpiGenericDeclK;
         it->list = &(region->decls);
         return true;
      case vhpiPortDecls:
         it->filter = vhpiPortDeclK;
         it->list = &(region->decls);
         return true;
      case vhpiStmts:
         it->list = &(region->stmts);
         return true;
      case vhpiBlockStmts:
         it->filter = vhpiBlockStmtK;
         it->list = &(region->stmts);
         return true;
      case vhpiCompInstStmts:
         it->filter = vhpiCompInstStmtK;
         it->list = &(region->stmts);
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
         it->list = &(etd->EnumLiterals);
         return true;
      }
      return false;
   }

   c_recordTypeDecl *record = is_recordTypeDecl(obj);
   if (record != NULL) {
      if (type == vhpiRecordElems) {
         it->list = &(record->RecordElems);
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

   c_objDecl *od = is_objDecl(obj);
   if (od != NULL) {
      switch(type) {
         case vhpiIndexedNames:
            if (!od->IndexedNames_valid) {
               c_typeDecl *td = vhpi_get_type(obj);
               vhpi_build_indexedNames(&(od->IndexedNames), obj, td);
               od->IndexedNames_valid = true;
            }
            it->list = &(od->IndexedNames);
            return true;
         case vhpiSelectedNames:
            if (!od->SelectedNames_valid) {
               c_typeDecl *td = vhpi_get_type(obj);
               vhpi_build_selectedNames(&(od->SelectedNames), obj, td);
               od->SelectedNames_valid = true;
            }
            it->list = &(od->SelectedNames);
            return true;
         default:
            return false;
      }
   }

   c_name *n = is_name(obj);
   if (n != NULL) {
      switch (type) {
      case vhpiIndexedNames:
         if (!n->IndexedNames_valid) {
            vhpi_build_indexedNames(&(n->IndexedNames), obj, n->expr.Type);
            n->IndexedNames_valid = true;
         }
         it->list = &(n->IndexedNames);
         return true;
      case vhpiSelectedNames:
         if (!n->SelectedNames_valid) {
            vhpi_build_selectedNames(&(n->SelectedNames), obj, n->expr.Type);
            n->SelectedNames_valid = true;
         }
         it->list = &(n->SelectedNames);
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

static void vhpi_do_callback(c_callback *cb)
{
   if (cb->State == vhpiEnable) {
      cb->status = CB_ACTIVE;
      (cb->data.cb_rtn)(&(cb->data));

      if (!vhpi_is_repetitive(cb->Reason))
         cb->State = vhpiMature;
   }

   if (cb->status == CB_FREE_LATER)
      free(cb);
   else
      cb->status = CB_INACTIVE;
}

static void vhpi_timeout_cb(rt_model_t *m, void *user)
{
   c_callback *cb;
   if ((cb = is_callback(user)))
      vhpi_do_callback(cb);
}

static void vhpi_signal_event_cb(uint64_t now, rt_signal_t *signal,
                                 rt_watch_t *watch, void *user)
{
   vhpiTimeT time;

   c_callback *cb;
   if ((cb = is_callback(user))) {
      if (cb->data.time) {
         vhpi_get_time(&time, NULL);
         cb->data.time = &time;
      }

      vhpi_do_callback(cb);
   }
}

static void vhpi_global_cb(rt_model_t *m, void *user)
{
   c_callback *cb;
   if ((cb = is_callback(user)))
      vhpi_do_callback(cb);
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

static void *vhpi_get_value_ptr(c_vhpiObject *obj)
{
   jit_t *j = vhpi_context()->jit;

   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL) {
      void *base = vhpi_get_value_ptr(pn->Prefix);
      if (base == NULL)
         return NULL;

      c_typeDecl *td = vhpi_get_type(pn->Prefix);
      if (td == NULL)
         return NULL;

      const jit_layout_t *l;
      switch (vhpi_get_prefix_kind(obj)) {
      case vhpiPortDeclK:
      case vhpiSigDeclK:
         l = signal_layout_of(td->type);
         break;
      default:
         l = layout_of(td->type);
         break;
      }

      c_selectedName *sn = is_selectedName(obj);
      if (sn != NULL) {
         assert(sn->Suffix->Position < l->nparts);
         return base + l->parts[sn->Suffix->Position].offset;
      }

      c_indexedName *in = is_indexedName(obj);
      if (in != NULL) {
         if (l->nparts == 2)
            return ((ffi_uarray_t *)base)->ptr;   // Wrapped array
         else {
            assert(l->nparts == 1);
            return base;
         }
      }

      vhpi_error(vhpiInternal, &(obj->loc), "unsupported prefixed name kind %s",
                 vhpi_class_str(obj->kind));
      return NULL;
   }

   c_objDecl *decl = cast_objDecl(obj);
   assert(decl != NULL);

   rt_scope_t *scope = vhpi_get_scope_abstractRegion(decl->decl.ImmRegion);
   if (*mptr_get(scope->privdata) == NULL) {
      vhpi_error(vhpiError, &(obj->loc), "%s has not been elaborated",
                 decl->decl.FullName);
      return NULL;
   }

   if (decl->decl.ImmRegion->handle == JIT_HANDLE_INVALID)
      decl->decl.ImmRegion->handle = jit_lazy_compile(j, scope->name);

   return jit_get_frame_var(j, decl->decl.ImmRegion->handle, decl->name);
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

////////////////////////////////////////////////////////////////////////////////
// Public API

DLLEXPORT
int vhpi_release_handle(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(handle));

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return 1;

   c_callback *cb = is_callback(obj);
   if (cb != NULL) {
      if (cb->status != CB_INACTIVE)
         cb->status = CB_FREE_LATER;
      else
         free(cb);
      return 0;
   }

   c_iterator *it = is_iterator(obj);
   if (it != NULL)
      free(it);

   return 0;
}

static int enable_cb(c_callback *cb)
{
   switch (cb->Reason) {
   case vhpiCbRepEndOfProcesses:
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbRepNextTimeStep:
   case vhpiCbEndOfProcesses:
   case vhpiCbStartOfSimulation:
   case vhpiCbEndOfSimulation:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbNextTimeStep:
   case vhpiCbEndOfTimeStep:
   case vhpiCbRepEndOfTimeStep:
   case vhpiCbEndOfInitialization:
      model_set_global_cb(vhpi_context()->model, vhpi_get_rt_event(cb->Reason),
                          vhpi_global_cb, cb);
      return 0;

   case vhpiCbAfterDelay:
      model_set_timeout_cb(vhpi_context()->model, cb->when,
                           vhpi_timeout_cb, cb);
      return 0;

   case vhpiCbValueChange:
      {
         c_vhpiObject *obj = from_handle(cb->data.obj);
         if (obj == NULL)
            return 1;

         c_objDecl *decl = cast_objDecl(obj);
         if (decl == NULL)
            return 1;

         rt_signal_t *signal = vhpi_get_signal_objDecl(decl);
         if (signal == NULL)
            return 1;

         cb->w = model_set_event_cb(vhpi_context()->model, signal,
                                    vhpi_signal_event_cb, cb, false);
         return 0;
      }

   default:
      vhpi_error(vhpiInternal, NULL,
                 "unsupported reason %d in vhpi_register_cb", cb->Reason);
      return 1;
   }
}

DLLEXPORT
vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_datap_p=%s flags=%x", cb_data_pp(cb_data_p), flags);

   c_callback *cb = new_object(sizeof(c_callback), vhpiCallbackK);
   cb->Reason = cb_data_p->reason;
   cb->State  = (flags & vhpiDisableCb) ? vhpiDisable : vhpiEnable;
   cb->data   = *cb_data_p;

   if (cb->Reason == vhpiCbAfterDelay) {
         if (cb->data.time == NULL) {
            vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
            goto err;
         }

         cb->when = vhpi_time_to_native(cb->data.time)
            + model_now(vhpi_context()->model, NULL);
   }
   else if (cb->Reason == vhpiCbValueChange && cb->data.value) {
      vhpi_error(vhpiInternal, NULL,
                 "values are not supported for Object callbacks");
      goto err;
   }

   if (!(flags & vhpiDisableCb) && enable_cb(cb))
      goto err;

   return (flags & vhpiReturnCb) ? handle_for(&(cb->object)) : NULL;

err:
   free(cb);
   return NULL;
}

static bool disable_cb(c_callback *cb)
{
   switch (cb->Reason) {
   case vhpiCbRepEndOfProcesses:
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbRepNextTimeStep:
   case vhpiCbEndOfProcesses:
   case vhpiCbStartOfSimulation:
   case vhpiCbEndOfSimulation:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbNextTimeStep:
   case vhpiCbEndOfTimeStep:
   case vhpiCbRepEndOfTimeStep:
      return model_clear_global_cb(vhpi_context()->model,
                                   vhpi_get_rt_event(cb->Reason),
                                   vhpi_global_cb, cb);
      return true;

   case vhpiCbAfterDelay:
      return model_clear_timeout_cb(vhpi_context()->model, cb->when,
                                    vhpi_timeout_cb, cb);

   case vhpiCbValueChange:
      return model_clear_event_cb(vhpi_context()->model, cb->w);

   default:
      assert(false);
      return true;
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

   VHPI_TRACE("cb.reason=%s", vhpi_cb_reason_str(cb->Reason));

   if (cb->State != vhpiEnable || (disable_cb(cb) && cb->status == CB_INACTIVE))
      free(cb);
   else {
      cb->State = vhpiDisable;
      cb->status = CB_FREE_LATER;
   }
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

   VHPI_TRACE("cb.reason=%s", vhpi_cb_reason_str(cb->Reason));

   if (cb->State != vhpiEnable) {
      vhpi_error(vhpiWarning, &(obj->loc),
                 "callback must be enabled in order to disable it");
      return 1;
   }

   disable_cb(cb);
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

   int ret = enable_cb(cb);
   if (!ret)
      cb->State = vhpiEnable;
   return ret;
}

DLLEXPORT
int vhpi_get_cb_info(vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
   VHPI_MISSING;
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
         if (td != NULL)
            return handle_for(&(td->BaseType->decl.object));

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
      }
      // Fall-through
   case vhpiType:
   case DEPRECATED_vhpiSubtype:
      {
         c_objDecl *d = cast_objDecl(obj);
         if (d == NULL)
            return NULL;

         if (type == vhpiBaseType) {
            c_typeDecl *td = d->Type->BaseType ?: d->Type;
            return handle_for(&(td->decl.object));
         }
         else
            return handle_for(&(d->Type->decl.object));
      }

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

         return handle_for(&(iu->DesignUnit->object));
      }
   case vhpiPrimaryUnit:
      return handle_for(&(cast_secondaryUnit(obj)->PrimaryUnit->object));
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
   case DEPRECATED_vhpiReturnTypeMark:
   case DEPRECATED_vhpiName:
   case DEPRECATED_vhpiTypeMark:
   case DEPRECATED_vhpiDecl:
      vhpi_error(vhpiError, &(obj->loc), "relationship %s is deprecated and "
                 "not implemented in vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;
   default:
      fatal_trace("relationship %s not supported in vhpi_handle",
                  vhpi_one_to_one_str(type));
   }
}

DLLEXPORT
vhpiHandleT vhpi_handle_by_name(const char *name, vhpiHandleT scope)
{
   vhpi_clear_error();

   VHPI_TRACE("name=%s scope=%p", name, scope);

   c_abstractRegion *region;
   if (scope == NULL)
      region = &(vhpi_context()->root->designInstUnit.region);
   else {
      c_vhpiObject *obj = from_handle(scope);
      if (obj == NULL)
         return NULL;

      region = cast_abstractRegion(obj);
      if (region == NULL)
         return NULL;

      expand_lazy_region(region);
   }

   char *copy LOCAL = xstrdup(name), *saveptr;
   char *elem = strtok_r(copy, ":.", &saveptr);

   if (scope == NULL && strcasecmp((char *)region->Name, elem) == 0)
      elem = strtok_r(NULL, ":.", &saveptr);

   if (elem == NULL)
      return handle_for(&(region->object));

 search_region:
   for (int i = 0; i < region->stmts.count; i++) {
      c_abstractRegion *r = is_abstractRegion(region->stmts.items[i]);
      if (r == NULL || strcasecmp((char *)r->Name, elem) != 0)
         continue;

      expand_lazy_region(r);

      if ((elem = strtok_r(NULL, ":.", &saveptr))) {
         region = r;
         goto search_region;
      }
      else
         return handle_for(&(r->object));
   }

   for (int i = 0; i < region->decls.count; i++) {
      c_abstractDecl *d = cast_abstractDecl(region->decls.items[i]);
      if (strcasecmp((char *)d->Name, elem) != 0)
         continue;

      char *suffix;
      c_vhpiObject *obj = &(d->object);
      while ((suffix = strtok_r(NULL, ".", &saveptr)) != NULL) {
         c_iterator it = {};
         if (!init_iterator(&it, vhpiSelectedNames, &(d->object)))
            return NULL;

         for (int i = 0; i < it.list->count; i++) {
            c_selectedName *sn = is_selectedName(it.list->items[i]);
            assert(sn != NULL);

            obj = &(sn->prefixedName.name.expr.object);
            if (strcasecmp((char *)sn->Suffix->decl.Name, suffix) == 0)
               return handle_for(obj);
         }

         return NULL;
      }

      return handle_for(obj);
   }

   return NULL;
}

DLLEXPORT
vhpiHandleT vhpi_handle_by_index(vhpiOneToManyT itRel,
                                 vhpiHandleT parent,
                                 int32_t index)
{
   vhpi_clear_error();

   VHPI_TRACE("itRel=%s parent=%s index=%d", vhpi_one_to_many_str(itRel),
              handle_pp(parent), index);

   c_vhpiObject *obj = from_handle(parent);
   if (obj == NULL)
      return NULL;

   c_iterator it = {};
   if (!init_iterator(&it, itRel, obj)) {
      vhpi_error(vhpiError, &(obj->loc),
                 "relation %s not supported in vhpi_handle_by_index",
                 vhpi_one_to_many_str(itRel));
      return NULL;
   }

   if (it.single ? index : index > it.list->count) {
      vhpi_error(vhpiError, &(obj->loc), "invalid %s index %d",
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

   c_vhpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return NULL;

   c_iterator *it = new_object(sizeof(c_iterator), vhpiIteratorK);
   if (!init_iterator(it, type, obj)) {
      vhpi_error(vhpiError, &(obj->loc),
                 "relation %s not supported in vhpi_iterator",
                 vhpi_one_to_many_str(type));
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
      vhpiHandleT handle = handle_for(it->list->items[it->pos++]);
      if (it->filter == vhpiUndefined
          || from_handle(handle)->kind == it->filter)
         return handle;
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

         VHPI_TRACE("left=%d right=%d", ir->LeftBound, ir->RightBound);
         if (property == vhpiLeftBoundP)
            return ir->LeftBound;
         else
            return ir->RightBound;
      }

   case vhpiIsUpP:
      {
         c_range *r = is_range(obj);
         if (r == NULL)
            goto missing_property;

         return r->IsUp;
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

         return etd->EnumLiterals.count;
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

         return rtd->RecordElems.count;
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

   c_designUnit *u = is_designUnit(obj);
   if (u != NULL) {
      switch (property) {
      case vhpiNameP: return u->Name;
      case vhpiCaseNameP: return u->CaseName;
      case vhpiFileNameP: return (vhpiCharT *)loc_file_str(&(u->object.loc));
      case vhpiUnitNameP: return u->UnitName;
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
                        vhpiHandleT object)
{
   VHPI_MISSING;
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

   int size = 1, num_elems = td->numElems;
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

   default:
      vhpi_error(vhpiError, &(obj->loc), "class kind %s cannot be used with "
                 "vhpi_get_value", vhpi_class_str(obj->kind));
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
#define SIGNAL_READ_ENUM(type) \
      scalar = ((const type *)value)[offset]
      FOR_ALL_SIZES(size, SIGNAL_READ_ENUM);
      break;
   case vhpiIntVal:
   case vhpiLongIntVal:
      scalar = ((const int32_t *)value)[offset];
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

#define SIGNAL_READ_ENUMV(type) do {                            \
            const type *p = ((const type *)value) + offset;     \
            for (int i = 0; i < value_p->numElems; i++)         \
               value_p->value.enumvs[i] = *p++;                 \
         } while (0)

         FOR_ALL_SIZES(size, SIGNAL_READ_ENUMV);
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
   rt_signal_t *signal;
   c_prefixedName *pn = is_prefixedName(obj);
   if (pn != NULL) {
      signal = vhpi_get_signal_prefixedName(pn);
      c_indexedName *in = is_indexedName(obj);
      if (in)
         offset = in->offset;
   }
   else {
      c_objDecl *decl = cast_objDecl(obj);
      if (decl == NULL)
         return 1;

      signal = vhpi_get_signal_objDecl(decl);
   }

   if (signal == NULL)
      return 1;

   rt_model_t *model = vhpi_context()->model;

   if (!model_can_create_delta(model)) {
      vhpi_error(vhpiError, &(obj->loc), "cannot create delta cycle "
                 "during current simulation phase");
      return 1;
   }

   switch (mode) {
   case vhpiForcePropagate:
   case vhpiDepositPropagate:
      {
         void *ext LOCAL = NULL, *ptr = NULL;
         uint8_t byte;
         union {
            uint8_t  uint8_t_val;
            uint16_t uint16_t_val;
            uint32_t uint32_t_val;
            uint64_t uint64_t_val;
            vhpiIntT vhpiIntT_val;
         } scalar;
         double real;
         int num_elems = 0;

         switch (value_p->format) {
         case vhpiLogicVal:
            num_elems = 1;
            byte = value_p->value.enumv;
            ptr = &byte;
            break;

         case vhpiSmallEnumVal:
            num_elems = 1;
            byte = value_p->value.smallenumv;
            ptr = &byte;
            break;

         case vhpiEnumVal:
            num_elems = 1;
            ptr = &scalar;

#define SIGNAL_WRITE_ENUM(type) do {                     \
               scalar.type##_val = value_p->value.enumv; \
            } while (0)

            FOR_ALL_SIZES(signal_size(signal), SIGNAL_WRITE_ENUM);
            break;

         case vhpiCharVal:
            num_elems = 1;
            byte = value_p->value.ch;
            ptr = &byte;
            break;

         case vhpiIntVal:
            num_elems = 1;
            scalar.vhpiIntT_val = value_p->value.intg;
            ptr = &scalar;   // Assume little endian
            break;

         case vhpiRealVal:
            num_elems = 1;
            real = value_p->value.real;
            ptr = &real;
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
               uint8_t size = signal_size(signal);
               ext = ptr = xmalloc_array(num_elems, size);

#define SIGNAL_WRITE_ENUMV(type) do { \
      for (int i = 0; i < num_elems; i++) \
         ((type *)ext)[i] = value_p->value.enumvs[i]; \
   } while (0)

               FOR_ALL_SIZES(size, SIGNAL_WRITE_ENUMV);
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

         default:
            vhpi_error(vhpiFailure, &(obj->loc), "value format "
                       "%d not supported in vhpi_put_value",
                       value_p->format);
            return 1;
         }

         if (offset + num_elems > signal_width(signal)) {
            vhpi_error(vhpiError, &(obj->loc),
                       "too many values (%d) for signal with width %"PRIu32,
                       num_elems, signal_width(signal));
            return 1;
         }

         if (mode == vhpiForcePropagate)
            force_signal(model, signal, ptr, offset, num_elems);
         else
            deposit_signal(model, signal, ptr, offset, num_elems);

         return 0;
      }

   case vhpiRelease:
      release_signal(model, signal, offset, signal_width(signal));
      return 0;

   default:
      vhpi_error(vhpiFailure, &(obj->loc), "mode %s not supported in "
                 "vhpi_put_value", vhpi_put_value_mode_str(mode));
      return 1;
   }
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
   VHPI_MISSING;
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
int vhpi_get_foreignf_info(vhpiHandleT hdl, vhpiForeignDataT *foreignDatap)
{
   VHPI_MISSING;
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
   c_physRange *pr = new_object(sizeof(c_physRange), vhpiPhysRangeK);
   init_range(&(pr->range), tree_subkind(t) == RANGE_TO);

   pr->PhysLeftBound  = vhpi_phys_from_native(assume_int(tree_left(t)));
   pr->PhysRightBound = vhpi_phys_from_native(assume_int(tree_right(t)));

   return pr;
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

   c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
   init_range(&(ir->range), dir == RANGE_TO);

   ir->LeftBound  = vhpi_int_from_native(left);
   ir->RightBound = vhpi_int_from_native(right);

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
   if (type_is_array(elem) && !type_has_ident(elem)) {
      // Anonymous subtype may need to access parent type to
      // get non-constant bounds
      td->ElemType = build_arrayTypeDecl(elem, decl, parent, obj,
                                         nextdim + td->NumDimensions);
   }
   else
      td->ElemType = cached_typeDecl(elem, NULL);

   if (type_is_unconstrained(type)) {
      for (int i = 0; i < td->NumDimensions; i++)
         APUSH(td->Constraints, &(build_unconstrained()->range.object));
   }
   else {
      td->composite.typeDecl.numElems = td->ElemType->numElems;

      tree_t c = type_constraint(type, 0);
      assert(tree_subkind(c) == C_INDEX);

      for (int i = 0; i < td->NumDimensions; i++) {
         tree_t r = tree_range(c, i);
         c_intRange *ir = build_int_range(r, parent, nextdim + i, obj);
         td->composite.typeDecl.numElems *= range_len(ir);
         APUSH(td->Constraints, &(ir->range.object));
      }
   }

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

      for (int i = 0; i < at->NumDimensions; i++) {
         const range_kind_t dir = ffi_array_dir(bounds[i].length);
         const int64_t left = bounds[i].left;
         const int64_t right =
            ffi_array_right(bounds[i].left, bounds[i].length);

         c_intRange *ir = new_object(sizeof(c_intRange), vhpiIntRangeK);
         init_range(&(ir->range), dir == RANGE_TO);

         ir->LeftBound  = vhpi_int_from_native(left);
         ir->RightBound = vhpi_int_from_native(right);

         td->typeDecl.numElems *= range_len(ir);
         APUSH(td->Constraints, &(ir->range.object));
      }
   }

   c_recordTypeDecl *rt = is_recordTypeDecl(&(base->decl.object));
   if (rt != NULL) {
      const jit_layout_t *l =
         is_signal ? signal_layout_of(base->type) :layout_of(base->type);

      for (int i = 0; i < rt->RecordElems.count; i++) {
         c_elemDecl *ed = is_elemDecl(rt->RecordElems.items[i]);
         assert(ed != NULL);

         if (ed->Type->IsUnconstrained) {
            void *fptr = ptr + l->parts[i].offset;
            c_typeDecl *sub = build_dynamicSubtype(ed->Type, fptr, kind);

            c_elemDecl *new = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
            init_elemDecl(new, ed->decl.tree, sub, ed->decl.ImmRegion);

            APUSH(td->Constraints, &(new->decl.object));
         }
         else
            APUSH(td->Constraints, &(ed->decl.object));
      }
   }

   return &(td->typeDecl);
}

static c_typeDecl *build_typeDecl(type_t type, c_vhpiObject *obj)
{
   type_t base = type;
   while (type_kind(base) == T_SUBTYPE && !type_has_ident(base))
      base = type_base(base);   // Anonymous subtypes have no declaration

   ident_t id = type_ident(base);
   tree_t unit = type_container(base);
   ident_t unit_name = tree_ident(unit);

   if (tree_kind(unit) == T_ELAB) {
      unit = tree_stmt(unit, 0);   // Get root block
      unit_name = ident_runtil(unit_name, '.');   // Strip ".elab"
   }

   assert(ident_starts_with(id, unit_name));

   // Skip library name
   ident_walk_selected(&unit_name);
   ident_walk_selected(&id);

   // Skip top-level unit name
   ident_walk_selected(&unit_name);
   ident_walk_selected(&id);

   assert(ident_walk_selected(&unit_name) == NULL);
   ident_t b = ident_walk_selected(&id);

   tree_t decl = NULL;
   while (id != NULL) {
      tree_t next = NULL;
      const int nstmts = tree_stmts(unit);
      for (int i = 0; next == NULL && i < nstmts; i++) {
         tree_t s = tree_stmt(unit, i);
         if (tree_ident(s) == b)
            next = s;
      }

      const int ndecls = tree_decls(unit);
      for (int i = 0; next == NULL && i < ndecls; i++) {
         tree_t d = tree_decl(unit, i);
         if (tree_ident(d) == b)
            next = d;
      }

      if (next == NULL)
         fatal_trace("cannot find %s within %s", istr(b),
                     istr(tree_ident(unit)));

      unit = next;
      b = ident_walk_selected(&id);
   }

   if ((decl = search_decls(unit, b, 0)) == NULL)
      fatal_trace("cannot find type declaration for %s", type_pp(type));

#ifdef DEBUG
   const tree_kind_t dkind = tree_kind(decl);
   if (type_kind(type) == T_SUBTYPE)
      assert(dkind == T_SUBTYPE_DECL || dkind == T_TYPE_DECL);
   else
      assert(dkind == T_TYPE_DECL);
#endif

   switch (type_kind(type)) {
   case T_INTEGER:
      {
         c_intTypeDecl *td =
            new_object(sizeof(c_intTypeDecl), vhpiIntTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);
         return &(td->scalar.typeDecl);
      }

   case T_ENUM:
      {
         c_enumTypeDecl *td =
            new_object(sizeof(c_enumTypeDecl), vhpiEnumTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);

         int nlits = type_enum_literals(type);
         for (int i = 0; i < nlits; i++) {
            c_enumLiteral *el =
               new_object(sizeof(c_enumLiteral), vhpiEnumLiteralK);
            init_enumLiteral(el, type_enum_literal(type, i), td);
            APUSH(td->EnumLiterals, &(el->decl.object));
         }
         return &(td->scalar.typeDecl);
      }

   case T_PHYSICAL:
      {
         c_physTypeDecl *td =
            new_object(sizeof(c_physTypeDecl), vhpiPhysTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);
         td->constraint = &(build_phys_range(range_of(type, 0))->range);
         return &(td->scalar.typeDecl);
      }

   case T_REAL:
      {
         c_floatTypeDecl *td =
            new_object(sizeof(c_intTypeDecl), vhpiFloatTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, type);
         return &(td->scalar.typeDecl);
      }

   case T_ARRAY:
      return build_arrayTypeDecl(type, decl, base, obj, 0);

   case T_RECORD:
      {
         c_recordTypeDecl *td =
            new_object(sizeof(c_recordTypeDecl), vhpiRecordTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl, type);

         int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            tree_t f = type_field(type, i);
            type_t ft = tree_type(f);

            c_elemDecl *ed = new_object(sizeof(c_elemDecl), vhpiElemDeclK);
            init_elemDecl(ed, f, NULL, td->composite.typeDecl.decl.ImmRegion);

            c_vhpiObject *fobj = NULL;
            if (!type_is_unconstrained(ft) && !type_const_bounds(ft)) {
               // Create a temporary selected name object so we can look
               // up the element bounds dynamically
               c_selectedName *sn =
                  new_object(sizeof(c_selectedName), vhpiSelectedNameK);
               init_selectedName(sn, obj, ed);
               fobj = &(sn->prefixedName.name.expr.object);

               // Must set the object type now due to circular references
               c_objDecl *od = cast_objDecl(obj);
               assert(od != NULL);
               od->Type = &(td->composite.typeDecl);
            }

            ed->Type = cached_typeDecl(ft, fobj);

            td->composite.typeDecl.numElems += ed->Type->numElems;
            APUSH(td->RecordElems, &(ed->decl.object));
         }

         return &(td->composite.typeDecl);
      }

   case T_SUBTYPE:
      if (type_is_array(type))
         return build_arrayTypeDecl(type, decl, base, obj, 0);
      else {
         c_subTypeDecl *td =
            new_object(sizeof(c_subTypeDecl), vhpiSubtypeDeclK);
         init_typeDecl(&(td->typeDecl), decl, type);
         td->typeDecl.BaseType = cached_typeDecl(type_base_recur(type), NULL);
         td->isResolved = type_has_resolution(type);

         unsigned nconstrs = type_constraints(type);
         if (nconstrs != 0) {
            assert(nconstrs == 1);

            tree_t c = type_constraint(type, 0);
            if (tree_subkind(c) != C_RANGE)
               fatal_trace("unsupported constraint subkind %d", tree_subkind(c));

            c_intRange *ir = build_int_range(tree_range(c, 0), NULL, 0, NULL);
            APUSH(td->Constraints, &(ir->range.object));
         }

         return &(td->typeDecl);
      }

   default:
      fatal_trace("cannot build VHPI typeDecl for %s %s",
                  type_kind_str(type_kind(type)), type_pp(type));
   }
}

static c_typeDecl *cached_typeDecl(type_t type, c_vhpiObject *obj)
{
   hash_t *cache = vhpi_context()->typecache;
   c_typeDecl *d = hash_get(cache, type);
   if (d == NULL) {
      d = build_typeDecl(type, obj);
      hash_put(cache, type, d);
   }

   return d;
}

static void build_genericDecl(tree_t generic, int pos,
                              c_abstractRegion *region)
{
   c_genericDecl *g = new_object(sizeof(c_genericDecl), vhpiGenericDeclK);
   init_interfaceDecl(&(g->interface), generic, pos, region);

   g->IsLocal = (tree_kind(region->tree) == T_COMPONENT);
   g->IsVital = false;
   g->Mode = mode_map[tree_subkind(generic)];

   APUSH(region->decls, &(g->interface.objDecl.decl.object));
}

static void build_portDecl(tree_t port, int pos,
                           c_abstractRegion *region)
{
   c_portDecl *p = new_object(sizeof(c_portDecl), vhpiPortDeclK);
   init_interfaceDecl(&(p->interface), port, pos, region);

   p->Mode = mode_map[tree_subkind(port)];

   APUSH(region->decls, &(p->interface.objDecl.decl.object));
}

static void build_signalDecl(tree_t decl, c_abstractRegion *region)
{
   c_sigDecl *s = new_object(sizeof(c_sigDecl), vhpiSigDeclK);
   init_objDecl(&(s->objDecl), decl, region);

   APUSH(region->decls, &(s->objDecl.decl.object));
}

static void build_constDecl(tree_t decl, c_abstractRegion *region)
{
   c_constDecl *cd = new_object(sizeof(c_constDecl), vhpiConstDeclK);
   init_objDecl(&(cd->objDecl), decl, region);

   cd->IsDeferred = !tree_has_value(decl);

   APUSH(region->decls, &(cd->objDecl.decl.object));
}

static c_abstractRegion *build_blockStmt(tree_t t, c_abstractRegion *region)
{
   c_blockStmt *bs = new_object(sizeof(c_blockStmt), vhpiBlockStmtK);
   init_abstractRegion(&(bs->region), t);
   init_stmt(&(bs->stmt), t);

   if (tree_decls(t) > 1) {
      tree_t d1 = tree_decl(t, 1);
      bs->IsGuarded = (tree_kind(d1) == T_IMPLICIT_SIGNAL
                       && tree_subkind(d1) == IMPLICIT_GUARD);
   }

   APUSH(region->InternalRegions, &(bs->region.object));
   APUSH(region->stmts, &(bs->region.object));

   return &(bs->region);
}

static c_abstractRegion *build_compInstStmt(tree_t t, tree_t inst,
                                            c_abstractRegion *region)
{
   c_compInstStmt *c = new_object(sizeof(c_compInstStmt), vhpiCompInstStmtK);
   init_designInstUnit(&(c->designInstUnit), t, NULL);
   init_stmt(&(c->stmt), t);

   APUSH(region->InternalRegions, &(c->designInstUnit.region.object));
   APUSH(region->stmts, &(c->designInstUnit.region.object));

   return &(c->designInstUnit.region);
}

static c_abstractRegion *build_forGenerate(tree_t t, c_abstractRegion *region)
{
   c_forGenerate *g = new_object(sizeof(c_forGenerate), vhpiForGenerateK);
   init_abstractRegion(&(g->region), t);
   init_stmt(&(g->stmt), t);

   APUSH(region->InternalRegions, &(g->region.object));
   APUSH(region->stmts, &(g->region.object));

   return &(g->region);
}

static void vhpi_build_decls(tree_t container, c_abstractRegion *region)
{
   const int ndecls = tree_decls(container);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
         build_signalDecl(d, region);
         break;
      case T_CONST_DECL:
         build_constDecl(d, region);
         break;
      default:
         break;
      }
   }
}

static void vhpi_build_ports(tree_t unit, c_abstractRegion *region)
{
   const int nports = tree_ports(unit);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(unit, i);
      build_portDecl(p, i, region);
   }
}

static void vhpi_build_generics(tree_t unit, c_abstractRegion *region)
{
   const int ngenerics = tree_generics(unit);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(unit, i);
      build_genericDecl(g, i, region);
   }
}

static void vhpi_build_stmts(tree_t container, c_abstractRegion *region)
{
   const int nstmts = tree_stmts(container);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(container, i);
      switch (tree_kind(s)) {
      case T_BLOCK:
         {
            tree_t h = tree_decl(s, 0);
            assert(tree_kind(h) == T_HIER);

            c_abstractRegion *r = NULL;
            switch (tree_subkind(h)) {
            case T_BLOCK:
               r = build_blockStmt(s, region);
               break;
            case T_ARCH:
               r = build_compInstStmt(s, tree_ref(h), region);
               break;
            case T_FOR_GENERATE:
               r = build_forGenerate(s, region);
               break;
            default:
               continue;
            }

            r->lazyfn = vhpi_lazy_block;
         }
         break;
      default:
         break;
      }
   }
}

static void vhpi_lazy_block(c_abstractRegion *r)
{
   vhpi_build_generics(r->tree, r);
   vhpi_build_ports(r->tree, r);
   vhpi_build_decls(r->tree, r);
   vhpi_build_stmts(r->tree, r);
}

static void vhpi_build_design_model(vhpi_context_t *c)
{
   const uint64_t start_us = get_timestamp_us();

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

   tree_t p = tree_primary(s);
   assert(tree_kind(p) == T_ENTITY);

   c_entityDecl *entity = new_object(sizeof(c_entityDecl), vhpiEntityDeclK);
   init_entityDecl(entity, p);

   c_secondaryUnit *arch = new_object(sizeof(c_secondaryUnit), vhpiArchBodyK);
   init_secondaryUnit(arch, s, &(entity->designUnit));

   c->root = new_object(sizeof(c_rootInst), vhpiRootInstK);
   init_designInstUnit(&(c->root->designInstUnit), b0, &(arch->designUnit));

   c_abstractRegion *region = &(c->root->designInstUnit.region);
   vhpi_build_generics(b0, region);
   vhpi_build_ports(b0, region);
   vhpi_build_decls(b0, region);
   vhpi_build_stmts(b0, region);

   VHPI_TRACE("building model for %s took %"PRIu64" ms",
              istr(ident_runtil(tree_ident(b0), '.')),
              (get_timestamp_us() - start_us) / 1000);
}

static void vhpi_initialise_cb(rt_model_t *m, void *arg)
{
   vhpi_context_t *c = arg;
   vhpi_build_design_model(c);
}

vhpi_context_t *vhpi_context_new(tree_t top, rt_model_t *model, jit_t *jit,
                                 int argc, char **argv)
{
   assert(global_context == NULL);

   vhpi_context_t *c = global_context = xcalloc(sizeof(vhpi_context_t));
   c->strtab    = shash_new(1024);
   c->model     = model;
   c->tool      = build_tool(argc, argv);
   c->typecache = hash_new(128);
   c->top       = top;
   c->jit       = jit;

   model_set_global_cb(model, RT_END_OF_INITIALISATION, vhpi_initialise_cb, c);

   return c;
}

void vhpi_context_free(vhpi_context_t *c)
{
   assert(c == global_context);
   global_context = NULL;

   shash_free(c->strtab);
   hash_free(c->typecache);
   free(c);
}
