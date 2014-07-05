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
#include "util.h"
#include "hash.h"
#include "tree.h"

#include <string.h>
#include <assert.h>
#include <dlfcn.h>
#include <stdarg.h>

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

static vhpi_obj_t *sim_cb_list;
static tree_t      top_level;
static hash_t     *handle_hash;

#define VHPI_MISSING fatal_trace("VHPI function %s not implemented", __func__)
#define VHPI_MAGIC   0xbadf00d

static inline vhpi_obj_t *vhpi_get_obj(vhpiHandleT handle)
{
   vhpi_obj_t *obj = (vhpi_obj_t *)handle;
   if (unlikely(obj->magic != VHPI_MAGIC))
      fatal_trace("bad magic on VHPI handle %p", handle);
   return obj;
}

static inline void vhpi_obj_assert(vhpi_obj_t *obj, vhpiClassKindT class)
{
   if (unlikely(obj->class != class))
      fatal_trace("expected VHPI class kind %d but have %d", class, obj->class);
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

int vhpi_assert(vhpiSeverityT severity, char *formatmsg,  ...)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   printf("vhpi_register_cb flags=%x reason=%d obj=%p time=%p\n",
          flags, cb_data_p->reason, cb_data_p->obj, cb_data_p->time);

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

   default:
      fatal("unsupported reason %d in vhpi_register_cb", cb_data_p->reason);
   }

   if (flags & vhpiReturnCb)
      return (vhpiHandleT)obj;
   else
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
   VHPI_MISSING;
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

   vhpi_obj_t *obj = vhpi_get_obj(handle);

   switch (property) {
   case vhpiStateP:
      vhpi_obj_assert(obj, vhpiCallbackK);
      if (obj->cb.fired)
         return vhpiMature;
      else if (obj->cb.enabled)
         return vhpiEnable;
      else
         return vhpiDisable;

   default:
      fatal_trace("unsupported property %d in vhpi_get", property);
   }
}

const vhpiCharT *vhpi_get_str(vhpiStrPropertyT property,
                              vhpiHandleT handle)
{
   printf("vhpi_get_str property=%d handle=%p\n", property, handle);

   switch (property) {
   case vhpiNameP:
      if (handle == NULL)
         return (vhpiCharT *)PACKAGE_NAME;
      else {
         vhpi_obj_t *obj = vhpi_get_obj(handle);
         assert(obj->kind == VHPI_TREE); // TODO: make error
         return (vhpiCharT *)istr(tree_ident(obj->tree));  // TODO: cache this
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
   VHPI_MISSING;
}

int vhpi_put_value(vhpiHandleT object,
                   vhpiValueT *value_p,
                   vhpiPutValueModeT mode)
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
   VHPI_MISSING;
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
   va_list ap;
   va_start(ap, format);

   const int ret = vhpi_vprintf(format, ap);

   va_end(ap);
   return ret;
}

int vhpi_vprintf(const char *format, va_list args)
{
   char *buf LOCAL = xvasprintf(format, args);
   notef("VHPI printf $green$%s$$", buf);
   return strlen(buf);
}

int vhpi_compare_handles(vhpiHandleT handle1, vhpiHandleT handle2)
{
   VHPI_MISSING;
}

int vhpi_check_error(vhpiErrorInfoT *error_info_p)
{
   return 0;  // TODO
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
   else if (ch < 160)
      return 0;
   else
      return 1;
}

void vhpi_load_plugins(tree_t top, const char *plugins)
{
   top_level = top;

   if (handle_hash != NULL)
      hash_free(handle_hash);

   handle_hash = hash_new(1024, true);

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

static void vhpi_sim_event(int reason)
{
   for (vhpi_obj_t *it = sim_cb_list; it != NULL; it = it->chain) {
      if ((it->cb.reason == reason) && it->cb.enabled) {
         (*it->cb.data.cb_rtn)(&(it->cb.data));
      }
   }
}

void vhpi_start_of_sim(void)
{
   vhpi_sim_event(vhpiCbStartOfSimulation);
}

void vhpi_end_of_sim(void)
{
   vhpi_sim_event(vhpiCbEndOfSimulation);
}
