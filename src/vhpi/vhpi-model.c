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
#include "opt.h"
#include "rt/rt.h"
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
   c_vhpiObject object;
   vhpiStringT  UnitName;
} c_designUnit;

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
} c_callback;

DEF_CLASS(callback, vhpiCallbackK, object);

static c_rootInst *rootInst = NULL;
static shash_t    *strtab = NULL;

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

static c_abstractDecl *cast_abstractDecl(c_vhpiObject *obj)
{
   switch (obj->kind) {
   case vhpiSigDeclK:
   case vhpiPortDeclK:
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
      vhpi_error(vhpiError, NULL, "class kind %s is not an object declaration",
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

   r->tree = t;
}

static void init_abstractDecl(c_abstractDecl *d, tree_t t, c_abstractRegion *r)
{
   const loc_t *loc = tree_loc(t);
   d->object.loc = *loc;

   d->LineNo     = loc->first_line;
   d->LineOffset = loc->line_delta;

   d->Name = d->CaseName = new_string(istr(tree_ident(t)));

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

static void init_typeDecl(c_typeDecl *d, tree_t t)
{
   init_abstractDecl(&(d->decl), t, NULL);
}

static void init_scalarTypeDecl(c_scalarTypeDecl *d, tree_t t)
{
   init_typeDecl(&(d->typeDecl), t);
   d->typeDecl.IsScalar = true;
}

static void init_compositeTypeDecl(c_compositeTypeDecl *d, tree_t t)
{
   init_typeDecl(&(d->typeDecl), t);
   d->typeDecl.IsComposite = true;
}

static void init_range(c_range *r, tree_t t)
{
   const range_kind_t rkind = tree_subkind(t);
   r->IsUp = (rkind == RANGE_TO);
}

static void vhpi_do_callback(c_callback *cb)
{
   if (cb->State == vhpiEnable) {
      (cb->data.cb_rtn)(&(cb->data));

      if (!vhpi_is_repetitive(cb->Reason))
         cb->State = vhpiMature;
   }
}

static void vhpi_timeout_cb(uint64_t now, void *user)
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

static void vhpi_global_cb(void *user)
{
   c_callback *cb;
   if ((cb = is_callback(user)))
      vhpi_do_callback(cb);
}

static rt_scope_t *vhpi_get_scope(c_abstractRegion *region)
{
   if (region->scope)
      return region->scope;

   rt_scope_t *scope = rt_find_scope(region->tree);
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

   rt_signal_t *signal = rt_find_signal(scope, decl->tree);
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

int vhpi_release_handle(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", handle_pp(handle));

   return 0;
}

vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_datap_p=%s flags=%x", cb_data_pp(cb_data_p), flags);

   c_callback *cb = new_object(sizeof(c_callback), vhpiCallbackK);
   cb->Reason = cb_data_p->reason;
   cb->State  = (flags & vhpiDisableCb) ? vhpiDisable : vhpiEnable;
   cb->data   = *cb_data_p;

   switch (cb_data_p->reason) {
   case vhpiCbRepEndOfProcesses:
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbRepNextTimeStep:
   case vhpiCbEndOfProcesses:
   case vhpiCbStartOfSimulation:
   case vhpiCbEndOfSimulation:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbNextTimeStep:
      rt_set_global_cb(vhpi_get_rt_event(cb_data_p->reason),
                       vhpi_global_cb, cb);
      break;

   case vhpiCbAfterDelay:
      if (cb_data_p->time == NULL) {
         vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
         goto failed;
      }

      rt_set_timeout_cb(vhpi_time_to_native(cb_data_p->time),
                        vhpi_timeout_cb, cb);
      break;

   case vhpiCbValueChange:
      {
         c_vhpiObject *obj = from_handle(cb_data_p->obj);
         if (obj == NULL)
            goto failed;

         c_abstractDecl *decl = cast_abstractDecl(obj);
         if (decl == NULL)
            goto failed;

         rt_signal_t *signal = vhpi_get_signal(decl);
         if (signal == NULL)
            goto failed;

         rt_set_event_cb(signal, vhpi_signal_event_cb, cb, false);
      }
      break;

   default:
      fatal("unsupported reason %d in vhpi_register_cb", cb_data_p->reason);
   }

   return (flags & vhpiReturnCb) ? handle_for(&(cb->object)) : NULL;

 failed:
   free(cb);
   return NULL;
}

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

   VHPI_MISSING;
}

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

   VHPI_MISSING;
}

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

   VHPI_MISSING;
}

int vhpi_get_cb_info(vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
   VHPI_MISSING;
}

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

   default:
      fatal_trace("relationship %s not supported in vhpi_handle",
                  vhpi_one_to_one_str(type));
   }
}

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

vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT handle)
{
   VHPI_TRACE("type=%s handle=%s", vhpi_one_to_many_str(type),
              handle_pp(handle));

   VHPI_MISSING;
}

vhpiHandleT vhpi_scan(vhpiHandleT iterator)
{
   VHPI_MISSING;
}

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
   case vhpiStaticnessP:
   default:
      vhpi_error(vhpiFailure, NULL, "unsupported property %s in vhpi_get",
                 vhpi_property_str(property));
      return vhpiUndefined;
   }
}

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

      return NULL;

   case vhpiFullNameP:
   case vhpiFullCaseNameP:
   case vhpiKindStrP:
   default:
      fatal_trace("unsupported property %s in vhpi_get_str",
                  vhpi_property_str(property));
   }
}

vhpiRealT vhpi_get_real(vhpiRealPropertyT property,
                        vhpiHandleT object)
{
   VHPI_MISSING;
}

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
         rt_signal_expand(signal, 0, &value, 1);

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
      case W_STD_LOGIC:
      case W_STD_ULOGIC:
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
               case W_STD_LOGIC:
               case W_STD_ULOGIC:
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
      const size_t need = rt_signal_string(signal, map_str,
                                           (char *)value_p->value.str,
                                           value_p->bufSize);
      if (need > value_p->bufSize)
         return need;
      else
         return 0;
   }
   else if (type_is_scalar(decl->type)) {
      uint64_t value;
      rt_signal_expand(signal, 0, &value, 1);

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
      value_p->numElems = rt_signal_expand(signal, 0, values, max);

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

            if (rt_can_create_delta())
               rt_force_signal(signal, &expanded, 1);
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

            rt_force_signal(signal, expanded, num_elems);
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

int vhpi_protected_call(vhpiHandleT varHdl,
                        vhpiUserFctT userFct,
                        void *userData)
{
   VHPI_MISSING;
}

int vhpi_schedule_transaction(vhpiHandleT drivHdl,
                              vhpiValueT *value_p,
                              uint32_t numValues,
                              vhpiTimeT *delayp,
                              vhpiDelayModeT delayMode,
                              vhpiTimeT *pulseRejp)
{
   VHPI_MISSING;
}

int vhpi_format_value(const vhpiValueT *in_value_p,
                      vhpiValueT *out_value_p)
{
   VHPI_MISSING;
}

void vhpi_get_time(vhpiTimeT *time_p, long *cycles)
{
   vhpi_clear_error();

   VHPI_TRACE("time_p=%p cycles=%p", time_p, cycles);

   unsigned deltas;
   const uint64_t now = rt_now(&deltas);

   if (time_p != NULL) {
      time_p->high = now >> 32;
      time_p->low  = now & 0xffffffff;
   }

   if (cycles != NULL)
      *cycles = deltas;
}

int vhpi_get_next_time(vhpiTimeT *time_p)
{
   VHPI_MISSING;
}

int vhpi_control(vhpiSimControlT command, ...)
{
   vhpi_clear_error();

   VHPI_TRACE("command=%d", command);

   switch (command) {
   case vhpiFinish:
   case vhpiStop:
      notef("VHPI plugin requested end of simulation");
      rt_stop();
      return 0;

   case vhpiReset:
      vhpi_error(vhpiFailure, NULL, "vhpiReset not supported");
      return 1;

   default:
      vhpi_error(vhpiFailure, NULL, "unsupported command in vhpi_control");
      return 1;
   }
}

vhpiHandleT vhpi_create(vhpiClassKindT kind,
                        vhpiHandleT handle1,
                        vhpiHandleT handle2)
{
   VHPI_MISSING;
}

int vhpi_get_foreignf_info(vhpiHandleT hdl, vhpiForeignDataT *foreignDatap)
{
   VHPI_MISSING;
}

size_t vhpi_get_data(int32_t id, void *dataLoc, size_t numBytes)
{
   VHPI_MISSING;
}

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
         init_scalarTypeDecl(&(td->scalar), decl);
         return &(td->scalar.typeDecl);
      }

   case T_ENUM:
      {
         c_enumTypeDecl *td =
            new_object(sizeof(c_enumTypeDecl), vhpiEnumTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl);
         td->NumLiterals = type_enum_literals(type);
         return &(td->scalar.typeDecl);
      }

   case T_PHYSICAL:
      {
         c_physTypeDecl *td =
            new_object(sizeof(c_physTypeDecl), vhpiPhysTypeDeclK);
         init_scalarTypeDecl(&(td->scalar), decl);
         td->constraint = &(build_phys_range(range_of(type, 0))->range);
         return &(td->scalar.typeDecl);
      }

   case T_ARRAY:
      {
         c_arrayTypeDecl *td =
            new_object(sizeof(c_arrayTypeDecl), vhpiArrayTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl);
         td->NumDimensions = type_index_constrs(type);
         return &(td->composite.typeDecl);
      }

   case T_RECORD:
      {
         c_recordTypeDecl *td =
            new_object(sizeof(c_recordTypeDecl), vhpiRecordTypeDeclK);
         init_compositeTypeDecl(&(td->composite), decl);
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
      h = hash_new(128, true);

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

void vhpi_build_design_model(tree_t top)
{
   const uint64_t start_us = get_timestamp_us();

   assert(tree_kind(top) == T_ELAB);

   tree_t b0 = tree_stmt(top, 0);
   assert(tree_kind(b0) == T_BLOCK);

   assert(strtab == NULL);
   strtab = shash_new(1024);

   rootInst = new_object(sizeof(c_rootInst), vhpiRootInstK);
   init_abstractRegion(&(rootInst->designInstUnit.region), b0);

   vhpi_build_decls(b0, &(rootInst->designInstUnit.region));
   vhpi_build_ports(b0, rootInst);

   VHPI_TRACE("building model for %s took %"PRIu64" ms",
              istr(ident_runtil(tree_ident(b0), '.')),
              (get_timestamp_us() - start_us) / 1000);
}
