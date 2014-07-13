//
//  Copyright (C) 2014  Nick Gasson
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
//  Additional permission under GNU GPL version 3 section 7
//
//  If you modify this Program, or any covered work, by linking or
//  combining it with the IEEE VHPI reference code (or a modified version
//  of that library), containing parts covered by the terms of the IEEE's
//  license, the licensors of this Program grant you additional permission
//  to convey the resulting work. Corresponding Source for a non-source
//  form of such a combination shall include the source code for the parts
//  of the IEEE VHPI reference code used as well as that of the covered work.
//

#include "vhpi_user.h"
#include "vhpi_priv.h"
#include "util.h"
#include "hash.h"
#include "tree.h"
#include "common.h"
#include "rt/rt.h"

#include <string.h>
#include <assert.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdlib.h>

typedef struct vhpi_cb  vhpi_cb_t;
typedef struct vhpi_obj vhpi_obj_t;

struct vhpi_cb {
   int         reason;
   bool        enabled;
   bool        fired;
   vhpiCbDataT data;
};

typedef enum {
   VHPI_CALLBACK,
   VHPI_TREE
} vhpi_obj_kind_t;

struct vhpi_obj {
   uint32_t        magic;
   vhpi_obj_t     *chain;
   vhpiClassKindT  class;
   vhpi_obj_kind_t kind;
   union {
      vhpi_cb_t cb;
      tree_t    tree;
   };
};

static vhpi_obj_t     *sim_cb_list;
static vhpi_obj_t     *end_proc_cb_list;
static tree_t          top_level;
static hash_t         *handle_hash;
static vhpiErrorInfoT  last_error;

static ident_t simple_name_i;
static ident_t std_logic_i;
static ident_t std_ulogic_i;

#define VHPI_MISSING fatal_trace("VHPI function %s not implemented", __func__)
#define VHPI_MAGIC   0xbadf00d

static void vhpi_clear_error(void)
{
   free(last_error.message);
   last_error.message  = NULL;
   last_error.severity = 0;
}

__attribute__((format(printf, 3, 4)))
static void vhpi_error(vhpiSeverityT sev, const loc_t *loc,
                       const char *fmt, ...)
{
   vhpi_clear_error();

   last_error.severity = sev;
   last_error.str = NULL;

   if (loc != NULL) {
      last_error.file = (char *)loc->file;
      last_error.line = loc->first_line;
   }

   va_list ap;
   va_start(ap, fmt);
   last_error.message = xvasprintf(fmt, ap);
   va_end(ap);

   if (loc != NULL)
      error_at(loc, "%s", last_error.message);
   else
      errorf("%s", last_error.message);
}

static uint64_t vhpi_time_to_native(const vhpiTimeT *time)
{
   return ((uint64_t)time->high << 32) | (uint64_t)time->low;
}

static vhpi_obj_t *vhpi_get_obj(vhpiHandleT handle, vhpi_obj_kind_t kind)
{
   vhpi_obj_t *obj = (vhpi_obj_t *)handle;
   assert(obj != NULL);
   if (unlikely(obj->magic != VHPI_MAGIC))
      vhpi_error(vhpiSystem, NULL, "bad magic on VHPI handle %p", handle);
   else if (unlikely(obj->kind != kind))
      vhpi_error(vhpiSystem, NULL, "expected VHPI object kind %d but have %d",
                 kind, obj->kind);
   return obj;
}

static vhpi_obj_t *vhpi_tree_to_obj(tree_t t, vhpiClassKindT class)
{
   vhpi_obj_t *obj = hash_get(handle_hash, t);
   if (obj == NULL) {
      obj = xmalloc(sizeof(vhpi_obj_t));
      memset(obj, '\0', sizeof(vhpi_obj_t));

      obj->magic = VHPI_MAGIC;
      obj->kind  = VHPI_TREE;
      obj->class = class;
      obj->tree  = t;
   }

   return obj;
}

static void vhpi_timeout_cb(uint64_t now, void *user)
{
   printf("vhpi_timeout_cb! now=%"PRIu64" user=%p\n", now, user);
}

static void vhpi_signal_event_cb(uint64_t now, tree_t sig,
                                 watch_t *watch, void *user)
{
   printf("vhpi_signal_event_cb! sig=%s user=%p\n",
          istr(tree_ident(sig)), user);
}

int vhpi_assert(vhpiSeverityT severity, char *formatmsg,  ...)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   printf("vhpi_register_cb flags=%x reason=%d obj=%p time=%p\n",
          flags, cb_data_p->reason, cb_data_p->obj, cb_data_p->time);

   vhpi_clear_error();

   vhpi_obj_t *obj = xmalloc(sizeof(vhpi_obj_t));
   memset(obj, '\0', sizeof(vhpi_obj_t));

   obj->class = vhpiCallbackK;
   obj->kind  = VHPI_CALLBACK;
   obj->chain = NULL;
   obj->magic = VHPI_MAGIC;

   obj->cb.reason  = cb_data_p->reason;
   obj->cb.enabled = !(flags & vhpiDisableCb);
   obj->cb.data    = *cb_data_p;

   switch (cb_data_p->reason) {
   case vhpiCbStartOfSimulation:
   case vhpiCbEndOfSimulation:
      obj->chain = sim_cb_list;
      sim_cb_list = obj;
      break;

   case vhpiCbAfterDelay:
      if (cb_data_p->time == NULL) {
         vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
         goto failed;
      }

      rt_set_timeout_cb(vhpi_time_to_native(cb_data_p->time),
                        vhpi_timeout_cb, obj);
      break;

   case vhpiCbEndOfProcesses:
      obj->chain = end_proc_cb_list;
      end_proc_cb_list = obj;
      break;

   case vhpiCbValueChange:
      {
         vhpi_obj_t *sig = vhpi_get_obj(cb_data_p->obj, VHPI_TREE);
         if (tree_kind(sig->tree) != T_SIGNAL_DECL) {
            vhpi_error(vhpiError, tree_loc(sig->tree),
                       "object %s is not a signal",
                       istr(tree_ident(sig->tree)));
            goto failed;
         }

         rt_set_event_cb(sig->tree, vhpi_signal_event_cb, obj);
      }
      break;

   default:
      fatal("unsupported reason %d in vhpi_register_cb", cb_data_p->reason);
   }

   if (flags & vhpiReturnCb)
      return (vhpiHandleT)obj;
   else
      return NULL;

 failed:
   free(obj);
   return NULL;
}

int vhpi_remove_cb(vhpiHandleT cb_obj)
{
   VHPI_MISSING;
}

int vhpi_disable_cb(vhpiHandleT cb_obj)
{
   VHPI_MISSING;
}

int vhpi_enable_cb(vhpiHandleT cb_obj)
{
   VHPI_MISSING;
}

int vhpi_get_cb_info(vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_handle_by_name(const char *name, vhpiHandleT scope)
{
   printf("vhpi_handle_by_name name=%s scope=%p\n", name, scope);

   vhpi_clear_error();

   if (scope == NULL) {
      const char *root_name = istr(tree_attr_str(top_level, simple_name_i)) + 1;
      if (strcmp(root_name, name) == 0)
         return (vhpiHandleT)vhpi_tree_to_obj(top_level, vhpiRootInstK);
      else
         return NULL;
   }

   vhpi_obj_t *obj = vhpi_get_obj(scope, VHPI_TREE);
   if (obj == NULL)
      return NULL;

   ident_t search = NULL;
   if (tree_kind(obj->tree) == T_ELAB)
      search = ident_prefix(tree_attr_str(obj->tree, simple_name_i),
                            ident_new(name), ':');
   else
      ident_prefix(tree_ident(obj->tree), ident_new(name), ':');

   printf("search=%s\n", istr(search));

   const int ndecls = tree_decls(top_level);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(top_level, i);
      if (tree_ident(d) == search)
         return (vhpiHandleT)vhpi_tree_to_obj(d, vhpiSigDeclK);
   }

   vhpi_error(vhpiError, NULL, "object %s not found", istr(search));
   return NULL;
}

vhpiHandleT vhpi_handle_by_index(vhpiOneToManyT itRel,
                                 vhpiHandleT parent,
                                 int32_t indx)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_handle(vhpiOneToOneT type, vhpiHandleT referenceHandle)
{
   printf("vhpi_handle type=%d referenceHandle=%p\n", type, referenceHandle);

   vhpi_clear_error();

   switch (type) {
   case vhpiRootInst:
   case vhpiDesignUnit:
      return (vhpiHandleT)vhpi_tree_to_obj(top_level, vhpiRootInstK);

   default:
      fatal_trace("type %d not supported in vhpi_handle", type);
   }
}

vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT referenceHandle)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_scan(vhpiHandleT iterator)
{
   VHPI_MISSING;
}

vhpiIntT vhpi_get(vhpiIntPropertyT property, vhpiHandleT handle)
{
   printf("vhpi_get property=%d object=%p\n", property, handle);

   vhpi_clear_error();

   switch (property) {
   case vhpiStateP:
      {
         vhpi_obj_t *obj = vhpi_get_obj(handle, VHPI_CALLBACK);
         if (obj->cb.fired)
            return vhpiMature;
         else if (obj->cb.enabled)
            return vhpiEnable;
         else
            return vhpiDisable;
      }

   case vhpiSizeP:
      {
         vhpi_obj_t *obj = vhpi_get_obj(handle, VHPI_TREE);
         return type_width(tree_type(obj->tree));
      }

   default:
      fatal_trace("unsupported property %d in vhpi_get", property);
   }
}

const vhpiCharT *vhpi_get_str(vhpiStrPropertyT property,
                              vhpiHandleT handle)
{
   printf("vhpi_get_str property=%d handle=%p\n", property, handle);

   vhpi_clear_error();

   switch (property) {
   case vhpiNameP:
      if (handle == NULL)
         return (vhpiCharT *)PACKAGE_NAME;
      else {
         vhpi_obj_t *obj = vhpi_get_obj(handle, VHPI_TREE);
         if (obj == NULL)
            return NULL;

         const char *full = NULL;
         if (tree_kind(obj->tree) == T_ELAB)
            full = istr(tree_attr_str(obj->tree, simple_name_i));
         else
            full = istr(tree_ident(obj->tree));

         const char *last_sep = strrchr(full, ':');
         if (last_sep == NULL)
            return (vhpiCharT *)full;
         else
            return (vhpiCharT *)(last_sep + 1);
      }

   case vhpiFullNameP:
      {
         vhpi_obj_t *obj = vhpi_get_obj(handle, VHPI_TREE);
         if (obj == NULL)
            return NULL;

         if (tree_kind(obj->tree) == T_ELAB)
            return (vhpiCharT *)istr(tree_attr_str(obj->tree, simple_name_i));
         else
            return (vhpiCharT *)istr(tree_ident(obj->tree));
      }

   case vhpiKindStrP:
      {
         vhpi_obj_t *obj = vhpi_get_obj(handle, VHPI_TREE);
         if (obj == NULL)
            return NULL;

         if (tree_kind(obj->tree) == T_ELAB)
            return (vhpiCharT *)"elaborated design";
         else if (class_has_type(class_of(obj->tree)))
            return (vhpiCharT *)type_pp(tree_type(obj->tree));
         else
            return (vhpiCharT *)tree_kind_str(tree_kind(obj->tree));
      }

   case vhpiToolVersionP:
      return (vhpiCharT *)PACKAGE_VERSION;

   default:
      fatal_trace("unsupported property %d in vhpi_get_str", property);
   }
}

vhpiRealT vhpi_get_real(vhpiRealPropertyT property,
                        vhpiHandleT object)
{
   VHPI_MISSING;
}

vhpiPhysT vhpi_get_phys(vhpiPhysPropertyT property,
                        vhpiHandleT object)
{
   VHPI_MISSING;
}

int vhpi_protected_call(vhpiHandleT varHdl,
                        vhpiUserFctT userFct,
                        void *userData)
{
   VHPI_MISSING;
}

int vhpi_get_value(vhpiHandleT expr, vhpiValueT *value_p)
{
   vhpi_clear_error();

   vhpi_obj_t *obj = vhpi_get_obj(expr, VHPI_TREE);

   if (tree_kind(obj->tree) != T_SIGNAL_DECL) {
      vhpi_error(vhpiInternal, tree_loc(obj->tree), "vhpi_get_value is only "
                 "supported for signal declaration objects");
      return 0;
   }

   type_t type = tree_type(obj->tree);
   type_t base = type_base_recur(type);

   printf("vhpi_get_value expr=%s type=%s\n",
          istr(tree_ident(obj->tree)), type_pp(type));

   ident_t type_name = type_ident(type);

   vhpiFormatT format;
   switch (type_kind(base)) {
   case T_ENUM:
      if ((type_name == std_logic_i) || (type_name == std_ulogic_i))
         format = vhpiLogicVal;
      else if (type_enum_literals(base) <= 256)
         format = vhpiSmallEnumVal;
      else
         format = vhpiEnumVal;
      break;

   case T_UARRAY:
   case T_CARRAY:
      {
         type_t elem = type_elem(base);
         switch (type_kind(elem)) {
         case T_ENUM:
            {
               ident_t elem_name = type_ident(elem);
               if ((elem_name == std_logic_i) || (elem_name == std_ulogic_i))
                  format = vhpiLogicVecVal;
               else if (type_enum_literals(elem) <= 256)
                  format = vhpiSmallEnumVecVal;
               else
                  format = vhpiEnumVecVal;
               break;
            }

         default:
            vhpi_error(vhpiInternal, tree_loc(obj->tree), "arrays of type %s "
                       "not supported in vhpi_get_value", type_pp(elem));
            return 0;
         }
      }
      break;

   default:
      vhpi_error(vhpiInternal, tree_loc(obj->tree), "type %s not supported in "
                 "vhpi_get_value", type_pp(type));
      return 0;
   }

   if (value_p->format == vhpiObjTypeVal)
      value_p->format = format;
   else if (value_p->format != format) {
      vhpi_error(vhpiError, tree_loc(obj->tree), "invalid format %d for object "
                 "%s: expecting %d", value_p->format,
                 istr(tree_ident(obj->tree)), format);
      return 0;
   }

   if (type_is_scalar(type)) {
      uint64_t value;
      rt_signal_value(obj->tree, &value, 1);

      switch (format) {
      case vhpiLogicVal:
      case vhpiEnumVal:
         value_p->value.enumv = value;
         return 0;

      case vhpiSmallEnumVal:
         value_p->value.smallenumv = value;
         return 0;

      default:
         assert(false);
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
      uint64_t *values LOCAL = xmalloc(sizeof(uint64_t) * max);
      value_p->numElems = rt_signal_value(obj->tree, values, max);

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
            assert(false);
         }
      }

      return 0;
   }
}

int vhpi_put_value(vhpiHandleT handle,
                   vhpiValueT *value_p,
                   vhpiPutValueModeT mode)
{
   vhpi_clear_error();

   vhpi_obj_t *obj = vhpi_get_obj(handle, VHPI_TREE);

   printf("vhpi_put_value object=%s mode=%d\n",
          istr(tree_ident(obj->tree)), mode);

   bool propagate = false;
   switch (mode) {
   case vhpiForcePropagate:
      propagate = true;
   case vhpiForce:
      {
         type_t type = tree_type(obj->tree);
         if (type_is_scalar(type)) {
            uint64_t expanded;
            switch (value_p->format) {
            case vhpiLogicVal:
            case vhpiEnumVal:
               expanded = value_p->value.enumv;
               break;

            case vhpiSmallEnumVal:
               expanded = value_p->value.smallenumv;
               break;

            default:
               vhpi_error(vhpiFailure, tree_loc(obj->tree), " value format %d "
                          "not supported in vhpi_put_value", value_p->format);
               return 1;
            }

            rt_force_signal(obj->tree, &expanded, 1, propagate);
         }
         else {
            uint64_t *expanded = NULL;
            int num_elems = 0;

            switch (value_p->format) {
            case vhpiLogicVecVal:
            case vhpiEnumVecVal:
               num_elems = value_p->bufSize / sizeof(vhpiEnumT);
               expanded = xmalloc(sizeof(uint64_t) * num_elems);
               for (int i = 0; i < num_elems; i++)
                  expanded[i] = value_p->value.enumvs[i];
               break;

            case vhpiSmallEnumVecVal:
               num_elems = value_p->bufSize / sizeof(vhpiSmallEnumT);
               expanded = xmalloc(sizeof(uint64_t) * num_elems);
               for (int i = 0; i < num_elems; i++)
                  expanded[i] = value_p->value.smallenumvs[i];
               break;

            default:
               vhpi_error(vhpiFailure, tree_loc(obj->tree), " value format %d "
                          "not supported in vhpi_put_value", value_p->format);
               return 1;
            }

            rt_force_signal(obj->tree, expanded, num_elems, propagate);
            free(expanded);
         }
         return 0;
      }

   default:
      vhpi_error(vhpiFailure, NULL, "mode %d not supported in vhpi_put_value",
                 mode);
      return 1;
   }
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
   VHPI_MISSING;
}

int vhpi_printf(const char *format, ...)
{
   vhpi_clear_error();

   va_list ap;
   va_start(ap, format);

   const int ret = vhpi_vprintf(format, ap);

   va_end(ap);
   return ret;
}

int vhpi_vprintf(const char *format, va_list args)
{
   vhpi_clear_error();

   char *buf LOCAL = xvasprintf(format, args);
   notef("VHPI printf $green$%s$$", buf);
   return strlen(buf);
}

int vhpi_compare_handles(vhpiHandleT handle1, vhpiHandleT handle2)
{
   vhpi_clear_error();

   return handle1 == handle2;
}

int vhpi_check_error(vhpiErrorInfoT *error_info_p)
{
   if (last_error.severity == 0)
      return 0;
   else {
      *error_info_p = last_error;
      return last_error.severity;
   }
}

int vhpi_release_handle(vhpiHandleT object)
{
   VHPI_MISSING;
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

int vhpi_is_printable(char ch)
{
   if (ch < 32)
      return 0;
   else if (ch < 127)
      return 1;
   else if (ch == 127)
      return 0;
   else if ((unsigned char)ch < 160)
      return 0;
   else
      return 1;
}

void vhpi_load_plugins(tree_t top, const char *plugins)
{
   simple_name_i = ident_new("simple_name");
   std_logic_i   = ident_new("IEEE.STD_LOGIC_1164.STD_LOGIC");
   std_ulogic_i  = ident_new("IEEE.STD_LOGIC_1164.STD_ULOGIC");

   top_level = top;

   if (handle_hash != NULL)
      hash_free(handle_hash);

   handle_hash = hash_new(1024, true);

   vhpi_clear_error();

   char *plugins_copy LOCAL = strdup(plugins);
   assert(plugins_copy);

   char *tok = strtok(plugins_copy, ",");
   do {
      notef("loading VHPI plugin %s", tok);

      void *handle = dlopen(tok, RTLD_LAZY);
      if (handle == NULL)
         fatal("%s", dlerror());

      (void)dlerror();
      void (**startup_funcs)() = dlsym(handle, "vhpi_startup_routines");
      const char *error = dlerror();
      if (error != NULL) {
         warnf("%s", error);
         dlclose(handle);
         continue;
      }

      while (*startup_funcs)
         (*startup_funcs++)();

   } while ((tok = strtok(NULL, ",")));
}

void vhpi_event(vhpi_event_t reason)
{
   for (vhpi_obj_t *it = sim_cb_list; it != NULL; it = it->chain) {
      if ((it->cb.reason == reason) && it->cb.enabled) {
         (*it->cb.data.cb_rtn)(&(it->cb.data));
      }
   }
}
