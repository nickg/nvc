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
#include "lib.h"
#include "option.h"
#include "rt/rt.h"
#include "rt/model.h"
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

typedef struct tag_abstractRegion c_abstractRegion;
typedef struct tag_expr c_expr;

typedef struct tag_abstractRegion {
   c_vhpiObject      object;
   tree_t            tree;
   rt_scope_t       *scope;
   vhpiObjectListT   decls;
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
} c_abstractRegion;

typedef struct {
   c_vhpiObject      object;
   type_t            type;
   tree_t            tree;
   rt_signal_t      *signal;
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
   c_abstractDecl decl;
   vhpiBooleanT   IsAnonymous;
   vhpiBooleanT   IsComposite;
   vhpiBooleanT   IsScalar;
   vhpiBooleanT   IsUnconstrained;
} c_typeDecl;

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
   vhpiIntT         NumLiterals;
} c_enumTypeDecl;

typedef struct {
   c_scalarTypeDecl scalar;
   c_range         *constraint;
} c_physTypeDecl;

DEF_CLASS(physTypeDecl, vhpiPhysTypeDeclK, scalar.typeDecl.decl.object);

typedef struct {
   c_compositeTypeDecl composite;
   vhpiIntT            NumDimensions;
} c_arrayTypeDecl;

typedef struct {
   c_compositeTypeDecl composite;
   vhpiIntT            NumFields;
} c_recordTypeDecl;

typedef struct {
   c_abstractDecl  decl;
   c_typeDecl     *BaseType;
   c_typeDecl     *Type;
   vhpiIntT        Access;
   vhpiBooleanT    IsDynamic;
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

typedef struct tag_expr {
   c_vhpiObject    object;
   vhpiStaticnessT Staticness;
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
   vhpiObjectListT  ports;
} c_rootInst;

DEF_CLASS(rootInst, vhpiRootInstK, designInstUnit.region.object);

typedef struct {
   c_vhpiObject object;
   vhpiStateT   State;
   vhpiEnumT    Reason;
   vhpiCbDataT  data;
   uint64_t     when;
} c_callback;

DEF_CLASS(callback, vhpiCallbackK, object);

static c_rootInst *rootInst = NULL;
static shash_t    *strtab = NULL;
static rt_model_t *model = NULL;

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
      return container_of(obj, c_abstractRegion, object);
   default:
      return NULL;
   }
}

static c_abstractRegion *cast_abstractRegion(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
      return container_of(obj, c_abstractRegion, object);
   default:
      vhpi_error(vhpiError, NULL, "class kind %s is not a region",
                 vhpi_class_str(obj->kind));
      return NULL;
   }
}

static c_abstractDecl *is_abstractDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigDeclK:
   case vhpiPortDeclK:
   case vhpiIntTypeDeclK:
   case vhpiEnumTypeDeclK:
   case vhpiPhysTypeDeclK:
   case vhpiArrayTypeDeclK:
   case vhpiRecordTypeDeclK:
      return container_of(obj, c_abstractDecl, object);
   default:
      return NULL;
   }
}

static c_abstractDecl *cast_abstractDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigDeclK:
   case vhpiPortDeclK:
   case vhpiIntTypeDeclK:
   case vhpiEnumTypeDeclK:
   case vhpiPhysTypeDeclK:
   case vhpiArrayTypeDeclK:
   case vhpiRecordTypeDeclK:
      return container_of(obj, c_abstractDecl, object);
   default:
      vhpi_error(vhpiError, NULL, "class kind %s is not a declaration",
                 vhpi_class_str(obj->kind));
      return NULL;
   }
}

static c_objDecl *cast_objDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigDeclK:
   case vhpiPortDeclK:
      return container_of(obj, c_objDecl, decl.object);
   default:
      vhpi_error(vhpiError, NULL, "class kind %s is not an object declaration",
                 vhpi_class_str(obj->kind));
      return NULL;
   }
}

static c_typeDecl *is_typeDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiIntTypeDeclK:
      return container_of(obj, c_typeDecl, decl.object);
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
      return container_of(obj, c_designUnit, object);
   default:
      return NULL;
   }
}

static c_designInstUnit *cast_designInstUnit(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiRootInstK:
      return container_of(obj, c_designInstUnit, region.object);
   default:
      vhpi_error(vhpiError, NULL, "class kind %s is not an instance of a design unit",
                 vhpi_class_str(obj->kind));
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

   switch (obj->kind) {
   case vhpiPortDeclK:
      tb_printf(tb, " Name=%s", cast_abstractDecl(obj)->Name);
      break;
   default:
      break;
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
   void *ptr = xcalloc(size);
   ((c_vhpiObject *)ptr)->kind = class;
   return ptr;
}

static vhpiCharT *new_string(const char *s)
{
   vhpiCharT *p = shash_get(strtab, s);
   if (p == NULL)
      shash_put(strtab, s, (p = (vhpiCharT *)xstrdup(s)));
   return p;
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

   r->tree = t;
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

static void init_objDecl(c_objDecl *d, tree_t t,
                         c_abstractRegion *ImmRegion,
                         c_typeDecl *BaseType)
{
   init_abstractDecl(&(d->decl), t, ImmRegion);

   d->BaseType = d->Type = BaseType;
}

static void init_interfaceDecl(c_interfaceDecl *d, tree_t t,
                               int Position,
                               c_abstractRegion *ImmRegion,
                               c_typeDecl *BaseType)
{
   init_objDecl(&(d->objDecl), t, ImmRegion, BaseType);

   d->Position = Position;
}

static void init_typeDecl(c_typeDecl *d, tree_t t, ident_t id)
{
   init_abstractDecl(&(d->decl), t, NULL);
   char *full LOCAL = xasprintf("@%s", istr(id));
   char *pos = full;
   while ((pos = strchr(pos, '.')))
      *pos = ':';
   d->decl.FullName = d->decl.FullCaseName = new_string(full);
}

static void init_scalarTypeDecl(c_scalarTypeDecl *d, tree_t t, ident_t id)
{
   init_typeDecl(&(d->typeDecl), t, id);
   d->typeDecl.IsScalar = true;
}

static void init_compositeTypeDecl(c_compositeTypeDecl *d, tree_t t, ident_t id)
{
   init_typeDecl(&(d->typeDecl), t, id);
   d->typeDecl.IsComposite = true;
}

static void init_range(c_range *r, tree_t t)
{
   const range_kind_t rkind = tree_subkind(t);
   r->IsUp = (rkind == RANGE_TO);
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

static void vhpi_do_callback(c_callback *cb)
{
   if (cb->State == vhpiEnable) {
      (cb->data.cb_rtn)(&(cb->data));

      if (!vhpi_is_repetitive(cb->Reason))
         cb->State = vhpiMature;
   }
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
   c_callback *cb;
   if ((cb = is_callback(user)))
      vhpi_do_callback(cb);
}

static void vhpi_global_cb(rt_model_t *m, void *user)
{
   c_callback *cb;
   if ((cb = is_callback(user)))
      vhpi_do_callback(cb);
}

static rt_scope_t *vhpi_get_scope(c_abstractRegion *region)
{
   if (region->scope)
      return region->scope;

   rt_scope_t *scope = find_scope(model, region->tree);
   if (scope == NULL) {
      vhpi_error(vhpiError, &(region->object.loc),
                 "cannot find scope object for %s", region->Name);
      return NULL;
   }

   region->scope = scope;
   return scope;
}

static rt_signal_t *vhpi_get_signal(c_abstractDecl *decl)
{
   if (decl->signal != NULL)
      return decl->signal;

   rt_scope_t *scope = vhpi_get_scope(decl->ImmRegion);
   if (scope == NULL)
      return NULL;

   rt_signal_t *signal = find_signal(scope, decl->tree);
   if (signal == NULL) {
      vhpi_error(vhpiError, &(decl->object.loc),
                 "cannot find signal object for %s", decl->Name);
      return NULL;
   }

   decl->signal = signal;
   return signal;
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

   switch (obj->kind) {
   case vhpiCallbackK:
      free(obj);
      return 0;
   default:
      return 0;
   }
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
      model_set_global_cb(model, vhpi_get_rt_event(cb->Reason),
                          vhpi_global_cb, cb);
      return 0;

   case vhpiCbAfterDelay:
      {
         if (cb->data.time == NULL) {
            vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
            return 1;
         }

         cb->when = vhpi_time_to_native(cb->data.time) + model_now(model, NULL);
         model_set_timeout_cb(model, cb->when, vhpi_timeout_cb, cb);
         return 0;
      }

   case vhpiCbValueChange:
      {
         c_vhpiObject *obj = from_handle(cb->data.obj);
         if (obj == NULL)
            return 1;

         c_abstractDecl *decl = cast_abstractDecl(obj);
         if (decl == NULL)
            return 1;

         rt_signal_t *signal = vhpi_get_signal(decl);
         if (signal == NULL)
            return 1;

         model_set_event_cb(model, signal, vhpi_signal_event_cb, cb, false);
         return 0;
      }

   default:
      fatal("unsupported reason %d in vhpi_register_cb", cb->Reason);
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

   if (enable_cb(cb)) {
      free(cb);
      return NULL;
   }

   return (flags & vhpiReturnCb) ? handle_for(&(cb->object)) : NULL;
}

static int disable_cb(c_callback *cb)
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
      model_clear_global_cb(model, vhpi_get_rt_event(cb->Reason),
                            vhpi_global_cb, cb);
      return 0;

   case vhpiCbAfterDelay:
      model_clear_timeout_cb(model, cb->when, vhpi_timeout_cb, cb);
      return 0;

   case vhpiCbValueChange:
      return 1;

   default:
      assert(false);
      return 1;
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

   int ret = disable_cb(cb);
   return ret;
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

   return disable_cb(cb);
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

   return enable_cb(cb);
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
      return handle_for(&(rootInst->designInstUnit.region.object));

   case DEPRECATED_vhpiSubtype:
   case DEPRECATED_vhpiReturnTypeMark:
   case DEPRECATED_vhpiName:
   case DEPRECATED_vhpiTypeMark:
   case DEPRECATED_vhpiDecl:
      vhpi_error(vhpiError, NULL, "relationship %s is deprecated and "
                 "not implemented in vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;

   case vhpiPrefix:
      return NULL;

   default:
      break;
   }

   c_vhpiObject *obj = from_handle(referenceHandle);
   if (obj == NULL)
      return NULL;

   switch (type) {
   case vhpiBaseType:
   case vhpiType:
      {
         c_objDecl *d = cast_objDecl(obj);
         if (d == NULL)
            return NULL;

         if (type == vhpiBaseType)
            return handle_for(&(d->BaseType->decl.object));
         else
            return handle_for(&(d->Type->decl.object));
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
      region = &(rootInst->designInstUnit.region);
   else {
      c_vhpiObject *obj = from_handle(scope);
      if (obj == NULL)
         return NULL;

      region = cast_abstractRegion(obj);
      if (region == NULL)
         return NULL;
   }

   char *copy LOCAL = xstrdup(name), *saveptr;
   char *elem = strtok_r(copy, ":", &saveptr);

   for (int i = 0; i < region->decls.count; i++) {
      c_abstractDecl *d = cast_abstractDecl(region->decls.items[i]);
      if (strcasecmp((char *)d->Name, elem) == 0)
         return handle_for(&(d->object));
   }

   c_rootInst *rootInst;
   if ((rootInst = is_rootInst(&(region->object)))) {
      for (int i = 0; i < rootInst->ports.count; i++) {
         c_abstractDecl *d = cast_abstractDecl(rootInst->ports.items[i]);
         if (strcasecmp((char *)d->Name, elem) == 0)
            return handle_for(&(d->object));
      }
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

   switch (itRel) {
   case vhpiConstraints:
      {
         c_physTypeDecl *ptd = is_physTypeDecl(obj);
         if (ptd != NULL && index == 0)
            return handle_for(&(ptd->constraint->object));

         vhpi_error(vhpiError, &(obj->loc), "invalid vhpiConstraints index %d",
                    index);
         return NULL;

      }

   default:
      fatal_trace("relation %s not supported in vhpi_handle_by_index",
                  vhpi_one_to_many_str(itRel));
   }
}

DLLEXPORT
vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT handle)
{
   VHPI_TRACE("type=%s handle=%s", vhpi_one_to_many_str(type),
              handle_pp(handle));

   VHPI_MISSING;
}

DLLEXPORT
vhpiHandleT vhpi_scan(vhpiHandleT iterator)
{
   VHPI_MISSING;
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
            return vhpiUndefined;

         return cb->State;
      }

   case vhpiKindP:
      return obj->kind;

   case vhpiIsCompositeP:
   case vhpiIsScalarP:
      {
         c_typeDecl *td = is_typeDecl(obj);
         if (td == NULL) {
            vhpi_error(vhpiError, &(obj->loc), "object is not a type");
            return vhpiUndefined;
         }

         if (property == vhpiIsCompositeP)
            return td->IsComposite;
         else
            return td->IsScalar;
      }

   case vhpiSizeP:
      {
         if (obj->kind != vhpiPortDeclK && obj->kind != vhpiSigDeclK) {
            vhpi_error(vhpiInternal, &(obj->loc), "vhpiSizeP is only "
                       "supported for signal and port objects");
            return 0;
         }

         c_abstractDecl *decl = cast_abstractDecl(obj);
         if (decl == NULL || decl->type == NULL)
            return 0;

         rt_signal_t *signal = vhpi_get_signal(decl);
         if (signal == NULL)
            return 0;

         return signal_width(signal);
      }
   case vhpiStaticnessP:
   default:
      vhpi_error(vhpiFailure, NULL, "unsupported property %s in vhpi_get",
                 vhpi_property_str(property));
      return vhpiUndefined;
   }
}

DLLEXPORT
const vhpiCharT *vhpi_get_str(vhpiStrPropertyT property, vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              handle_pp(handle));

   switch (property) {
   case vhpiToolVersionP:
      return (vhpiCharT *)PACKAGE_VERSION;

   case vhpiNameP:
   case vhpiCaseNameP:
      if (handle == NULL)
         return (vhpiCharT *)PACKAGE_NAME;

      break;

   case vhpiFullNameP:
   case vhpiFullCaseNameP:
   case vhpiKindStrP:
   case vhpiUnitNameP:
      break;

   default:
      goto unsupported;
   }

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
         case vhpiFullNameP: return region->FullName;
         case vhpiFullCaseNameP: return region->FullCaseName;
         default: goto unsupported;
         }
   }

   c_abstractDecl *d = is_abstractDecl(obj);
   if (d != NULL) {
      switch (property) {
      case vhpiNameP: return d->Name;
      case vhpiCaseNameP: return d->CaseName;
      case vhpiFullNameP: return d->FullName;
      case vhpiFullCaseNameP: return d->FullCaseName;
      default: goto unsupported;
      }
   }

   c_designUnit *u = is_designUnit(obj);
   if (u != NULL) {
      switch (property) {
      case vhpiNameP: return u->Name;
      case vhpiCaseNameP: return u->CaseName;
      case vhpiUnitNameP: return u->UnitName;
      default: goto unsupported;
      }
   }

unsupported:
   fatal_trace("unsupported property %s in vhpi_get_str",
               vhpi_property_str(property));
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

         c_physTypeDecl *td = cast_physTypeDecl(&(decl->BaseType->decl.object));
         if (td == NULL)
            return invalid;

         rt_signal_t *signal = vhpi_get_signal(&(decl->decl));
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

DLLEXPORT
int vhpi_get_value(vhpiHandleT expr, vhpiValueT *value_p)
{
   vhpi_clear_error();

   VHPI_TRACE("expr=%s value_p=%p", handle_pp(expr), value_p);

   c_vhpiObject *obj = from_handle(expr);
   if (obj == NULL)
      return -1;

   if (obj->kind != vhpiPortDeclK && obj->kind != vhpiSigDeclK) {
      vhpi_error(vhpiInternal, &(obj->loc), "vhpi_get_value is only "
                 "supported for signal and port objects");
      return -1;
   }

   c_abstractDecl *decl = cast_abstractDecl(obj);
   if (decl == NULL)
      return -1;

   type_t base = type_base_recur(decl->type);
   ident_t type_name = type_ident(decl->type);

   vhpiFormatT format;
   switch (type_kind(base)) {
   case T_ENUM:
      switch (is_well_known(type_name)) {
      case W_IEEE_LOGIC:
      case W_IEEE_ULOGIC:
      case W_STD_BIT:
         if (value_p->format == vhpiBinStrVal)
            format = value_p->format;
         else
            format = vhpiLogicVal;
         break;
      default:
         if (type_enum_literals(base) <= 256)
            format = vhpiSmallEnumVal;
         else
            format = vhpiEnumVal;
         break;
      }
      break;

   case T_INTEGER:
      format = vhpiIntVal;
      break;

   case T_ARRAY:
      {
         type_t elem = type_elem(base);
         switch (type_kind(elem)) {
         case T_ENUM:
            {
               switch (is_well_known(type_ident(elem))) {
               case W_IEEE_LOGIC:
               case W_IEEE_ULOGIC:
               case W_STD_BIT:
                  if (value_p->format == vhpiBinStrVal)
                     format = value_p->format;
                  else
                     format = vhpiLogicVecVal;
                  break;
               default:
                  if (type_enum_literals(elem) <= 256)
                     format = vhpiSmallEnumVecVal;
                  else
                     format = vhpiEnumVecVal;
                  break;
               }
               break;
            }

         default:
            vhpi_error(vhpiInternal, &(obj->loc), "arrays of type %s "
                       "not supported in vhpi_get_value", type_pp(elem));
            return -1;
         }
      }
      break;

   default:
      vhpi_error(vhpiInternal, &(obj->loc), "type %s not supported in "
                 "vhpi_get_value", type_pp(decl->type));
      return -1;
   }

   if (value_p->format == vhpiObjTypeVal)
      value_p->format = format;
   else if (value_p->format != format) {
      vhpi_error(vhpiError, &(obj->loc), "invalid format %d for "
                 "object %s: expecting %d", value_p->format,
                 decl->Name, format);
      return -1;
   }

   rt_signal_t *signal = vhpi_get_signal(decl);
   if (signal == NULL)
      return -1;

   if (format == vhpiBinStrVal) {
      const char *map_str = vhpi_map_str_for_type(decl->type);
      const size_t need = signal_string(signal, map_str,
                                        (char *)value_p->value.str,
                                        value_p->bufSize);
      if (need > value_p->bufSize)
         return need;
      else
         return 0;
   }
   else if (type_is_scalar(decl->type)) {
      uint64_t value;
      signal_expand(signal, &value, 1);

      switch (format) {
      case vhpiLogicVal:
      case vhpiEnumVal:
         value_p->value.enumv = value;
         return 0;

      case vhpiSmallEnumVal:
         value_p->value.smallenumv = value;
         return 0;

      case vhpiIntVal:
         value_p->value.intg = value;
         return 0;

      default:
         vhpi_error(vhpiError, &(obj->loc), "unsupported format %d",
                    value_p->format);
         return -1;
      }
   }
   else {
      size_t elemsz = 0;
      switch (format) {
      case vhpiLogicVecVal:
      case vhpiEnumVecVal:
         elemsz = sizeof(vhpiEnumT);
         break;
      case vhpiSmallEnumVal:
         elemsz = sizeof(vhpiSmallEnumT);
         break;
      default:
         assert(false);
      }

      const int max = value_p->bufSize / elemsz;
      uint64_t *values LOCAL = xmalloc_array(max, sizeof(uint64_t));
      value_p->numElems = signal_expand(signal, values, max);

      const int copy = MIN(value_p->numElems, max);

      for (int i = 0; i < copy; i++) {
         switch (format) {
         case vhpiLogicVecVal:
         case vhpiEnumVecVal:
            value_p->value.enumvs[i] = values[i];
            break;
         case vhpiSmallEnumVal:
            value_p->value.smallenumvs[i] = values[i];
            break;
         default:
            vhpi_error(vhpiError, &(obj->loc), "unsupported format %d", format);
         }
      }

      return 0;
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

   c_abstractDecl *decl = cast_abstractDecl(obj);
   if (decl == NULL)
      return 1;

   rt_signal_t *signal = vhpi_get_signal(decl);
   if (signal == NULL)
      return 1;

   switch (mode) {
   case vhpiForcePropagate:
      {
         if (type_is_scalar(decl->type)) {
            uint64_t expanded;
            switch (value_p->format) {
            case vhpiLogicVal:
            case vhpiEnumVal:
               expanded = value_p->value.enumv;
               break;

            case vhpiSmallEnumVal:
               expanded = value_p->value.smallenumv;
               break;

            case vhpiIntVal:
               expanded = value_p->value.intg;
               break;

            default:
               vhpi_error(vhpiFailure, &(obj->loc), "value format "
                          "%d not supported in vhpi_put_value",
                          value_p->format);
               return 1;
            }

            if (model_can_create_delta(model))
               force_signal(signal, &expanded, 1);
            else {
               vhpi_error(vhpiError, &(obj->loc), "cannot force "
                          "propagate signal during current simulation phase");
               return 1;
            }
         }
         else {
            uint64_t *expanded = NULL;
            int num_elems = 0;

            switch (value_p->format) {
            case vhpiLogicVecVal:
            case vhpiEnumVecVal:
               num_elems = value_p->bufSize / sizeof(vhpiEnumT);
               expanded = xmalloc_array(num_elems, sizeof(uint64_t));
               for (int i = 0; i < num_elems; i++)
                  expanded[i] = value_p->value.enumvs[i];
               break;

            case vhpiSmallEnumVecVal:
               num_elems = value_p->bufSize / sizeof(vhpiSmallEnumT);
               expanded = xmalloc_array(num_elems, sizeof(uint64_t));
               for (int i = 0; i < num_elems; i++)
                  expanded[i] = value_p->value.smallenumvs[i];
               break;

            default:
               vhpi_error(vhpiFailure, &(obj->loc), " value format "
                          "%d not supported in vhpi_put_value",
                          value_p->format);
               return 1;
            }

            force_signal(signal, expanded, num_elems);
            free(expanded);
         }
         return 0;
      }

   default:
      vhpi_error(vhpiFailure, NULL, "mode %s not supported in vhpi_put_value",
                 vhpi_put_value_mode_str(mode));
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
   const uint64_t now = model_now(model, &deltas);

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
      model_stop(model);
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
   init_range(&(pr->range), t);

   pr->PhysLeftBound  = vhpi_phys_from_native(assume_int(tree_left(t)));
   pr->PhysRightBound = vhpi_phys_from_native(assume_int(tree_right(t)));

   return pr;
}

static c_typeDecl *build_typeDecl(type_t type)
{
   ident_t id = type_ident(type);
   tree_t unit = lib_get_qualified(ident_runtil(id, '.')), decl = NULL;

   if (unit != NULL)
      decl = search_decls(unit, ident_rfrom(id, '.'), 0);

   if (decl == NULL)
      fatal_trace("cannot find type declaration for %s", istr(id));

   assert(tree_kind(decl) == T_TYPE_DECL);

   switch (type_kind(type)) {
   case T_INTEGER:
      {
         c_intTypeDecl *td =
            new_object(sizeof(c_intTypeDecl), vhpiIntTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, id);
         return &(td->scalar.typeDecl);
      }

   case T_ENUM:
      {
         c_enumTypeDecl *td =
            new_object(sizeof(c_enumTypeDecl), vhpiEnumTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, id);
         td->NumLiterals = type_enum_literals(type);
         return &(td->scalar.typeDecl);
      }

   case T_PHYSICAL:
      {
         c_physTypeDecl *td =
            new_object(sizeof(c_physTypeDecl), vhpiPhysTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl, id);
         td->constraint = &(build_phys_range(range_of(type, 0))->range);
         return &(td->scalar.typeDecl);
      }

   case T_ARRAY:
      {
         c_arrayTypeDecl *td =
            new_object(sizeof(c_arrayTypeDecl), vhpiArrayTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl, id);
         td->NumDimensions = type_index_constrs(type);
         return &(td->composite.typeDecl);
      }

   case T_RECORD:
      {
         c_recordTypeDecl *td =
            new_object(sizeof(c_recordTypeDecl), vhpiRecordTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl, id);
         td->NumFields = type_fields(type);
         return &(td->composite.typeDecl);
      }

   default:
      fatal_trace("cannot build VHPI typeDecl for %s %s",
                  type_kind_str(type_kind(type)), type_pp(type));
   }
}

static c_typeDecl *cached_typeDecl(type_t type)
{
   static hash_t *h = NULL;
   if (h == NULL)
      h = hash_new(128);

   c_typeDecl *d = hash_get(h, type);
   if (d == NULL) {
      d = build_typeDecl(type);
      hash_put(h, type, d);
   }

   return d;
}

static c_vhpiObject *vhpi_build_signal_decl(tree_t decl,
                                            c_abstractRegion *region)
{
   c_typeDecl *td = cached_typeDecl(type_base_recur(tree_type(decl)));

   c_sigDecl *s = new_object(sizeof(c_sigDecl), vhpiSigDeclK);
   init_objDecl(&(s->objDecl), decl, region, td);

   return &(s->objDecl.decl.object);
}

static void vhpi_build_decls(tree_t container, c_abstractRegion *region)
{
   const int ndecls = tree_decls(container);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
         APUSH(region->decls, vhpi_build_signal_decl(d, region));
         break;
      default:
         break;
      }
   }
}

static c_vhpiObject *vhpi_build_port_decl(tree_t port, int pos,
                                          c_abstractRegion *region)
{
   c_typeDecl *td = cached_typeDecl(type_base_recur(tree_type(port)));

   c_portDecl *p = new_object(sizeof(c_portDecl), vhpiPortDeclK);
   init_interfaceDecl(&(p->interface), port, pos, region, td);
   return &(p->interface.objDecl.decl.object);
}

static void vhpi_build_ports(tree_t unit, c_rootInst *where)
{
   c_abstractRegion *region = &(where->designInstUnit.region);

   const int nports = tree_ports(unit);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(unit, i);
      APUSH(where->ports, vhpi_build_port_decl(p, i, region));
   }
}

void vhpi_build_design_model(tree_t top, rt_model_t *m)
{
   const uint64_t start_us = get_timestamp_us();

   assert(tree_kind(top) == T_ELAB);

   tree_t b0 = tree_stmt(top, 0);
   if (tree_kind(b0) == T_VERILOG)
      fatal_trace("verilog top-level modules are not supported by VHPI");
   assert(tree_kind(b0) == T_BLOCK);

   tree_t h = tree_decl(b0, 0);
   assert(tree_kind(h) == T_HIER);

   tree_t s = tree_ref(h);
   assert(tree_kind(s) == T_ARCH);

   tree_t p = tree_primary(s);
   assert(tree_kind(p) == T_ENTITY);

   assert(strtab == NULL);
   strtab = shash_new(1024);

   model = m;

   c_entityDecl *entity = new_object(sizeof(c_entityDecl), vhpiEntityDeclK);
   init_entityDecl(entity, p);

   c_secondaryUnit *arch = new_object(sizeof(c_secondaryUnit), vhpiArchBodyK);
   init_secondaryUnit(arch, s, &(entity->designUnit));

   rootInst = new_object(sizeof(c_rootInst), vhpiRootInstK);
   init_designInstUnit(&(rootInst->designInstUnit), b0, &(arch->designUnit));
   APUSH(rootInst->regions, &(rootInst->designInstUnit.region.object));

   vhpi_build_decls(b0, &(rootInst->designInstUnit.region));
   vhpi_build_ports(b0, rootInst);

   VHPI_TRACE("building model for %s took %"PRIu64" ms",
              istr(ident_runtil(tree_ident(b0), '.')),
              (get_timestamp_us() - start_us) / 1000);
}
