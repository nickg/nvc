//
//  Copyright (C) 2014-2018  Nick Gasson
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
#include "common.h"
#include "rt/rt.h"

#include <string.h>
#include <assert.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdlib.h>

#ifdef __MINGW32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

typedef struct vhpi_cb  vhpi_cb_t;
typedef struct vhpi_obj vhpi_obj_t;

struct vhpi_cb {
   int         reason;
   bool        enabled;
   bool        fired;
   bool        repetitive;
   bool        released;
   vhpiCbDataT data;
   int         list_pos;
   bool        has_handle;
};

typedef enum {
   VHPI_CALLBACK,
   VHPI_TREE,
   VHPI_TYPE,
   VHPI_RANGE
} vhpi_obj_kind_t;

#define VHPI_ANY (vhpi_obj_kind_t)-1

struct vhpi_obj {
   uint32_t        magic;
   vhpiClassKindT  class;
   vhpi_obj_kind_t kind;
   unsigned        refcnt;
   vhpi_cb_t       cb;
   union {
      tree_t  tree;
      type_t  type;
      void   *pointer;
      range_t range;
   };
};

typedef struct {
   vhpi_obj_t **objects;
   unsigned     num;
   unsigned     max;
} cb_list_t;

static cb_list_t       cb_list;
static tree_t          top_level;
static hash_t         *handle_hash;
static vhpiErrorInfoT  last_error;
static bool            trace_on = false;

const vhpiPhysT vhpiFS = { 0, 1 };
const vhpiPhysT vhpiPS = { 0, 0x3e8 };
const vhpiPhysT vhpiNS = { 0, 0xf4240 };
const vhpiPhysT vhpiUS = { 0, 0x3b9aca00 };
const vhpiPhysT vhpiMS = { 0xe8, 0xd4a51000 };
const vhpiPhysT vhpiS = { 0x38d7e, 0xa4c68000 };
const vhpiPhysT vhpiMN = { 0xd529ae, 0x9e860000 };
const vhpiPhysT vhpiHR = { 0x31f5c4ed, 0x27680000 };

const char *vhpi_property_str(int property);
const char *vhpi_cb_reason_str(int reason);
const char *vhpi_one_to_one_str(vhpiOneToOneT kind);
const char *vhpi_one_to_many_str(vhpiOneToManyT kind);

#define VHPI_MISSING fatal_trace("VHPI function %s not implemented", __func__)
#define VHPI_MAGIC   0xbadf00d

#define vhpi_validate_handle(handle, kind) \
   _vhpi_validate_handle(handle, kind, __func__)

#define VHPI_TRACE(...) do {                            \
      if (unlikely(trace_on))                           \
         vhpi_trace(__func__, __VA_ARGS__);             \
   } while (0)

static void vhpi_clear_error(void)
{
   last_error.severity = 0;
}

__attribute__((format(printf, 2, 3)))
static void vhpi_trace(const char *func, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   fprintf(stderr, "VHPI: %s ", func);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");

   va_end(ap);
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

   free(last_error.message);

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

static const char *vhpi_obj_kind_str(vhpi_obj_kind_t kind)
{
   const char *names[] = { "callback", "tree", "type", "range" };
   if ((unsigned int)kind > ARRAY_LEN(names))
      return "???";
   else
      return names[kind];
}

static bool _vhpi_validate_handle(vhpiHandleT handle, vhpi_obj_kind_t kind,
                                  const char *func)
{
   vhpi_obj_t *obj = (vhpi_obj_t *)handle;
   if (unlikely(obj == NULL)) {
      vhpi_error(vhpiSystem, NULL, "unexpected null handle in %s", func);
      return NULL;
   }
   else if (unlikely(obj->magic != VHPI_MAGIC)) {
      vhpi_error(vhpiSystem, NULL, "bad magic 0x%x on VHPI handle %p in %s",
                 obj->magic, obj, func);
      return NULL;
   }
   else if ((kind != VHPI_ANY) && (obj->kind != kind)) {
      vhpi_error(vhpiSystem, NULL, "expected VHPI object kind '%s' "
                 "but have '%s' in %s", vhpi_obj_kind_str(kind),
                 vhpi_obj_kind_str(obj->kind), func);
      return NULL;
   }
   else
      return obj;
}

static const char *vhpi_cb_data_str(const vhpiCbDataT *data)
{
   static char buf[256];
   checked_sprintf(buf, sizeof(buf), "{reason=%s cb_rtn=%p user_data=%p}",
                   vhpi_cb_reason_str(data->reason), data->cb_rtn,
                   data->user_data);
   return buf;
}

static const char *vhpi_pretty_handle(vhpiHandleT handle)
{
   static char *buf = NULL;
   free(buf);
   buf = NULL;

   if (handle == NULL)
      return "<null>";
   else if (handle->magic != VHPI_MAGIC)
      return "<BAD>";

   switch (handle->kind) {
   case VHPI_TREE:
      return (buf = xasprintf("<tree %s>",
                              tree_kind_str(tree_kind(handle->tree))));
   case VHPI_TYPE:
      return (buf = xasprintf("<type %s>", type_pp(handle->type)));

   case VHPI_CALLBACK:
      return (buf = xasprintf("<callback enabled=%d data=%s>",
                              handle->cb.enabled,
                              vhpi_cb_data_str(&(handle->cb.data))));

   case VHPI_RANGE:
      return "<range>";
   }

   return "<\?\?\?>";
}

static void vhpi_free_obj(vhpi_obj_t *obj)
{
   obj->magic = 0xbadc0de;
   free(obj);
}

static void vhpi_remember_cb(cb_list_t *list, vhpi_obj_t *obj)
{
   if (unlikely(list->objects == NULL)) {
      list->max = 64;
      list->objects = xmalloc(list->max * sizeof(vhpi_obj_t));
      memset(list->objects, '\0', list->max * sizeof(vhpi_obj_t));
   }
   else if (unlikely(list->num == list->max)) {
      const unsigned oldmax = list->max;
      list->max *= 2;
      list->objects = xrealloc(list->objects, list->max * sizeof(vhpi_obj_t));
      memset(list->objects + oldmax, '\0',
             (list->max - oldmax)  * sizeof(vhpi_obj_t));
   }

   for (unsigned i = list->num;
        i != (list->num - 1) % list->max;
        i = (i + 1) % list->max) {
      if (list->objects[i] == NULL) {
         list->objects[i] = obj;
         obj->cb.list_pos = i;
         (list->num)++;
         return;
      }
   }

   assert(false);
}

static void vhpi_forget_cb(cb_list_t *list, vhpi_obj_t *obj)
{
   assert((obj->cb.list_pos >= 0) && (obj->cb.list_pos < list->max));
   assert(list->objects[obj->cb.list_pos] == obj);
   list->objects[obj->cb.list_pos] = NULL;
   obj->cb.list_pos = -1;
}

static int vhpi_count_live_cbs(cb_list_t *list)
{
   int count = 0;
   for (unsigned i = 0; (i < list->max) && (count < list->num); i++) {
      if ((list->objects[i] != NULL) && list->objects[i]->cb.has_handle)
         count++;
   }

   return count;
}

static void vhpi_check_for_leaks(void)
{
   int leak_tree = 0, leak_cb = 0, leak_type = 0, leak_cons = 0;

   if (handle_hash != NULL) {
      hash_iter_t now = HASH_BEGIN;
      const void *key;
      void *value;
      while (hash_iter(handle_hash, &now, &key, &value)) {
         if (value != NULL) {
            vhpi_obj_t *obj = value;
            switch (obj->kind) {
            case VHPI_TREE:
               leak_tree += obj->refcnt;
               break;
            case VHPI_TYPE:
               leak_type += obj->refcnt;
               break;
            case VHPI_RANGE:
               leak_cons += obj->refcnt;
               break;
            default:
               break;
            }
         }
      }
   }

   leak_cb += vhpi_count_live_cbs(&cb_list);

   if (leak_tree > 0 || leak_cb > 0 || leak_type > 0) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "VHPI plugin leaked");
      if (leak_tree > 0)
         tb_printf(tb, " %d tree handles", leak_tree);
      if (leak_type > 0)
         tb_printf(tb, "%s%s %d type handles", leak_tree ? "," : "",
                   leak_tree && !leak_cons ? " and" : "", leak_type);
      if (leak_cons > 0)
         tb_printf(tb, "%s%s %d constraint handles", leak_type ? "," : "",
                   leak_type && !leak_cb ? " and" : "", leak_cons);
      if (leak_cb > 0)
         tb_printf(tb, "%s%s %d callback handles",
                   leak_tree || leak_type ? "," : "",
                   leak_tree ? " and" : "", leak_cb);

      warnf("%s", tb_get(tb));
   }
}

static vhpi_obj_t *vhpi_tree_to_obj(tree_t t, vhpiClassKindT class)
{
   vhpi_obj_t *obj = hash_get(handle_hash, t);
   if (obj == NULL) {
      obj = xcalloc(sizeof(vhpi_obj_t));
      obj->magic  = VHPI_MAGIC;
      obj->kind   = VHPI_TREE;
      obj->class  = class;
      obj->tree   = t;
      obj->refcnt = 1;

      hash_put(handle_hash, t, obj);
   }
   else {
      assert(obj->refcnt > 0);
      (obj->refcnt)++;
   }

   return obj;
}

static vhpi_obj_t *vhpi_type_to_obj(type_t t)
{
   vhpi_obj_t *obj = hash_get(handle_hash, t);
   if (obj == NULL) {
      obj = xcalloc(sizeof(vhpi_obj_t));
      obj->magic  = VHPI_MAGIC;
      obj->kind   = VHPI_TYPE;
      obj->type   = t;
      obj->refcnt = 1;

      hash_put(handle_hash, t, obj);
   }
   else {
      assert(obj->refcnt > 0);
      (obj->refcnt)++;
   }

   return obj;
}

static vhpi_obj_t *vhpi_range_to_obj(range_t r)
{
   vhpi_obj_t *obj = hash_get(handle_hash, r.left /* XXX */);
   if (obj == NULL) {
      obj = xcalloc(sizeof(vhpi_obj_t));
      obj->magic  = VHPI_MAGIC;
      obj->kind   = VHPI_RANGE;
      obj->range  = r;
      obj->refcnt = 1;

      hash_put(handle_hash, r.left, obj);
   }
   else {
      assert(obj->refcnt > 0);
      (obj->refcnt)++;
   }

   return obj;
}

static void vhpi_fire_event(vhpi_obj_t *obj)
{
   if (obj->cb.released) {
      // This handle has already been released by vhpi_release_handle
      assert(obj->cb.list_pos == -1);
      vhpi_free_obj(obj);
   }
   else if (obj->cb.enabled && (!obj->cb.fired || obj->cb.repetitive)) {
      // Handle may be released by callback so take care not to
      // reference it afterwards
      const bool release = !obj->cb.has_handle && !obj->cb.repetitive;
      obj->cb.fired = true;
      (*obj->cb.data.cb_rtn)(&(obj->cb.data));
      if (release)
         vhpi_release_handle((vhpiHandleT)obj);
   }
}

static void vhpi_timeout_cb(uint64_t now, void *user)
{
   if (vhpi_validate_handle(user, VHPI_CALLBACK))
      vhpi_fire_event((vhpiHandleT)user);
}

static void vhpi_signal_event_cb(uint64_t now, tree_t sig,
                                 watch_t *watch, void *user)
{
   if (vhpi_validate_handle(user, VHPI_CALLBACK))
      vhpi_fire_event((vhpiHandleT)user);
}

static void vhpi_global_cb(void *user)
{
   if (vhpi_validate_handle(user, VHPI_CALLBACK))
      vhpi_fire_event((vhpiHandleT)user);
}

static const char *vhpi_map_str_for_type(type_t type)
{
   ident_t type_name;
   if (type_is_array(type))
      type_name = type_ident(type_elem(type));
   else
      type_name = type_ident(type);

   if ((type_name == std_logic_i) || (type_name == std_ulogic_i))
      return "UX01ZWLH-";
   else if (type_name == std_bit_i)
      return "01";
   else
      assert(false);
}

static rt_event_t vhpi_get_rt_event(int reason)
{
   switch (reason){
   case vhpiCbNextTimeStep:
   case vhpiCbRepNextTimeStep:
      return RT_NEXT_TIME_STEP;
   case vhpiCbRepEndOfProcesses:
   case vhpiCbEndOfProcesses:
      return RT_END_OF_PROCESSES;
   case vhpiCbStartOfSimulation:
      return RT_START_OF_SIMULATION;
   case vhpiCbEndOfSimulation:
      return RT_END_OF_SIMULATION;
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbLastKnownDeltaCycle:
      return RT_LAST_KNOWN_DELTA_CYCLE;
   default:
      assert(false);
   }
}

int vhpi_assert(vhpiSeverityT severity, char *formatmsg,  ...)
{
   vhpi_clear_error();

   VHPI_TRACE("severity=%d formatmsg=\"%s\"", severity, formatmsg);

   va_list ap;
   va_start(ap, formatmsg);
   char *buf LOCAL = xvasprintf(formatmsg, ap);
   va_end(ap);

   switch (severity) {
   case vhpiNote:
      notef("%s", buf);
      break;

   case vhpiWarning:
      warnf("%s", buf);
      break;

   case vhpiError:
      errorf("%s", buf);
      break;

   case vhpiFailure:
   case vhpiSystem:
   case vhpiInternal:
      fatal("%s", buf);
      break;
   }

   return 0;
}

vhpiHandleT vhpi_register_cb(vhpiCbDataT *cb_data_p, int32_t flags)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_datap_p=%s flags=%x", vhpi_cb_data_str(cb_data_p), flags);

   vhpi_obj_t *obj = xmalloc(sizeof(vhpi_obj_t));
   memset(obj, '\0', sizeof(vhpi_obj_t));

   obj->class = vhpiCallbackK;
   obj->kind  = VHPI_CALLBACK;
   obj->magic = VHPI_MAGIC;

   obj->cb.reason     = cb_data_p->reason;
   obj->cb.enabled    = !(flags & vhpiDisableCb);
   obj->cb.data       = *cb_data_p;
   obj->cb.has_handle = !!(flags & vhpiReturnCb);
   obj->cb.list_pos   = -1;

   switch (cb_data_p->reason) {
   case vhpiCbRepEndOfProcesses:
   case vhpiCbRepLastKnownDeltaCycle:
   case vhpiCbRepNextTimeStep:
      obj->cb.repetitive = true;
      (obj->cb.reason)--;   // Non-repetitive constant
      // Fall-through
   case vhpiCbEndOfProcesses:
   case vhpiCbStartOfSimulation:
   case vhpiCbEndOfSimulation:
   case vhpiCbLastKnownDeltaCycle:
   case vhpiCbNextTimeStep:
      rt_set_global_cb(vhpi_get_rt_event(cb_data_p->reason),
                       vhpi_global_cb, obj);
      vhpi_remember_cb(&cb_list, obj);
      break;

   case vhpiCbAfterDelay:
      if (cb_data_p->time == NULL) {
         vhpi_error(vhpiError, NULL, "missing time for vhpiCbAfterDelay");
         goto failed;
      }

      rt_set_timeout_cb(vhpi_time_to_native(cb_data_p->time),
                        vhpi_timeout_cb, obj);

      vhpi_remember_cb(&cb_list, obj);
      break;

   case vhpiCbValueChange:
      {
         if (!vhpi_validate_handle(cb_data_p->obj, VHPI_TREE))
            goto failed;

         if (tree_kind(cb_data_p->obj->tree) != T_SIGNAL_DECL) {
            vhpi_error(vhpiError, tree_loc(cb_data_p->obj->tree),
                       "object %s is not a signal",
                       istr(tree_ident(cb_data_p->obj->tree)));
            goto failed;
         }

         obj->tree = cb_data_p->obj->tree;
         obj->cb.repetitive = true;

         rt_set_event_cb(cb_data_p->obj->tree, vhpi_signal_event_cb,
                         obj, false);

         vhpi_remember_cb(&cb_list, obj);
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
   vhpi_free_obj(obj);
   return NULL;
}

int vhpi_remove_cb(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", vhpi_pretty_handle(handle));

   return vhpi_release_handle(handle);
}

int vhpi_disable_cb(vhpiHandleT cb_obj)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_obj=%s", vhpi_pretty_handle(cb_obj));

   if (!vhpi_validate_handle(cb_obj, VHPI_CALLBACK))
      return 1;

   cb_obj->cb.enabled = false;
   return 0;
}

int vhpi_enable_cb(vhpiHandleT cb_obj)
{
   vhpi_clear_error();

   VHPI_TRACE("cb_obj=%s", vhpi_pretty_handle(cb_obj));

   if (!vhpi_validate_handle(cb_obj, VHPI_CALLBACK))
      return 1;

   cb_obj->cb.enabled = true;
   return 0;
}

int vhpi_get_cb_info(vhpiHandleT object, vhpiCbDataT *cb_data_p)
{
   VHPI_MISSING;
}

vhpiHandleT vhpi_handle_by_name(const char *name, vhpiHandleT scope)
{
   vhpi_clear_error();

   VHPI_TRACE("name=%s scope=%p", name, scope);

   tree_t root = NULL;

   if (scope == NULL) {
      const char *root_name = istr(tree_attr_str(top_level, simple_name_i)) + 1;
      if (strcmp(root_name, name) == 0)
         return (vhpiHandleT)vhpi_tree_to_obj(top_level, vhpiRootInstK);

      const char *dot = strchr(name, '.');
      if (dot == NULL)
         return NULL;

      if (strncmp(root_name, name, dot - name) != 0)
         return NULL;

      root = top_level;
      name = dot + 1;
   }
   else {
      if (!vhpi_validate_handle(scope, VHPI_TREE))
         return NULL;

      root = scope->tree;
   }

   ident_t search = NULL;
   if (tree_kind(root) == T_ELAB)
      search = ident_prefix(tree_attr_str(root, simple_name_i),
                            ident_new(name), ':');
   else
      search = ident_prefix(tree_ident(root), ident_new(name), ':');

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
                                 int32_t index)
{
   vhpi_clear_error();

   VHPI_TRACE("itRel=%s parent=%p index=%d", vhpi_one_to_many_str(itRel),
              parent, index);

   switch (itRel) {
   case vhpiConstraints:
      {
         if (!vhpi_validate_handle(parent, VHPI_TYPE))
            return NULL;

         if (index < 0 || index >= type_dims(parent->type)) {
            vhpi_error(vhpiError, NULL, "invalid constaint index %d for "
                       "type %s", index, type_pp(parent->type));
            return NULL;
         }

         return vhpi_range_to_obj(type_dim(parent->type, index));
      }

   default:
      fatal_trace("relation %s not supported in vhpi_handle_by_index",
                  vhpi_one_to_many_str(itRel));
   }
}

vhpiHandleT vhpi_handle(vhpiOneToOneT type, vhpiHandleT referenceHandle)
{
   vhpi_clear_error();

   VHPI_TRACE("type=%s referenceHandle=%p", vhpi_one_to_one_str(type),
              referenceHandle);

   switch (type) {
   case vhpiRootInst:
   case vhpiDesignUnit:
      return vhpi_tree_to_obj(top_level, vhpiRootInstK);

   case vhpiBaseType:
   case vhpiType:
      if (!vhpi_validate_handle(referenceHandle, VHPI_TREE))
         return NULL;
      else if (!class_has_type(class_of(referenceHandle->tree))) {
         vhpi_error(vhpiError, tree_loc(referenceHandle->tree),
                    "object does not have relationship %s in vhpi_handle",
                    vhpi_one_to_one_str(type));
         return NULL;
      }
      else {
         type_t ty = tree_type(referenceHandle->tree);
         if (type == vhpiBaseType)
            return vhpi_type_to_obj(type_base_recur(ty));
         else
            return vhpi_type_to_obj(ty);
      }

   case DEPRECATED_vhpiSubtype:
   case DEPRECATED_vhpiReturnTypeMark:
   case DEPRECATED_vhpiName:
   case DEPRECATED_vhpiTypeMark:
   case DEPRECATED_vhpiDecl:
      vhpi_error(vhpiError, NULL, "relationship %s is deprecated and "
                 "not implemented in vhpi_handle", vhpi_one_to_one_str(type));
      return NULL;

   default:
      fatal_trace("relationship %s not supported in vhpi_handle",
                  vhpi_one_to_one_str(type));
   }
}

vhpiHandleT vhpi_iterator(vhpiOneToManyT type, vhpiHandleT handle)
{
   VHPI_TRACE("type=%s handle=%s", vhpi_one_to_many_str(type),
              vhpi_pretty_handle(handle));

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
              vhpi_pretty_handle(handle));

   switch (property) {
   case vhpiStateP:
      {
         if (!vhpi_validate_handle(handle, VHPI_CALLBACK))
            return vhpiUndefined;

         if (handle->cb.fired && !handle->cb.repetitive)
            return vhpiMature;
         else if (handle->cb.enabled)
            return vhpiEnable;
         else
            return vhpiDisable;
      }

   case vhpiSizeP:
      {
         if (!vhpi_validate_handle(handle, VHPI_TREE))
            return vhpiUndefined;

         return type_width(tree_type(handle->tree));
      }

   case vhpiKindP:
      {
         if (!vhpi_validate_handle(handle, VHPI_TREE))
            return vhpiUndefined;

         switch (tree_kind(handle->tree)) {
         case T_PORT_DECL:
            return vhpiPortDeclK;

         case T_SIGNAL_DECL:
            if (tree_attr_int(handle->tree, fst_dir_i, -1) == -1)
               return vhpiSigDeclK;
            else
               return vhpiPortDeclK;

         case T_ELAB:
            return vhpiRootInstK;

         default:
            vhpi_error(vhpiFailure, tree_loc(handle->tree), "cannot convert "
                       "tree kind %s to vhpiClassKindT",
                       tree_kind_str(tree_kind(handle->tree)));
         }
      }

   case vhpiIsCompositeP:
      if (!vhpi_validate_handle(handle, VHPI_TYPE))
         return vhpiUndefined;
      else
         return type_is_array(handle->type) || type_is_record(handle->type);

   case vhpiIsScalarP:
      if (!vhpi_validate_handle(handle, VHPI_TYPE))
         return vhpiUndefined;
      else
         return type_is_scalar(handle->type);

   case vhpiStaticnessP:
      if (!vhpi_validate_handle(handle, VHPI_TREE))
         return vhpiUndefined;
      else {
         vhpi_error(vhpiError, tree_loc(handle->tree), "property "
                    "vhpiStaticnessP not yet implemented in vhpi_get");
         return vhpiUndefined;
      }

   default:
      vhpi_error(vhpiFailure, NULL, "unsupported property %s in vhpi_get",
                 vhpi_property_str(property));
      return vhpiUndefined;
   }
}

const vhpiCharT *vhpi_get_str(vhpiStrPropertyT property,
                              vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("property=%s handle=%s", vhpi_property_str(property),
              vhpi_pretty_handle(handle));

   switch (property) {
   case vhpiNameP:
   case vhpiCaseNameP:
      if (handle == NULL)
         return (vhpiCharT *)PACKAGE_NAME;
      else {
         if (!vhpi_validate_handle(handle, VHPI_TREE))
            return NULL;

         const char *full = NULL;
         if (tree_kind(handle->tree) == T_ELAB)
            full = istr(tree_attr_str(handle->tree, simple_name_i));
         else
            full = istr(tree_ident(handle->tree));

         const char *last_sep = strrchr(full, ':');
         if (last_sep == NULL)
            return (vhpiCharT *)full;
         else
            return (vhpiCharT *)(last_sep + 1);
      }

   case vhpiFullNameP:
   case vhpiFullCaseNameP:
      {
         if (!vhpi_validate_handle(handle, VHPI_TREE))
            return NULL;

         if (tree_kind(handle->tree) == T_ELAB)
            return (vhpiCharT *)istr(tree_attr_str(handle->tree,
                                                   simple_name_i));
         else
            return (vhpiCharT *)istr(tree_ident(handle->tree));
      }

   case vhpiKindStrP:
      {
         if (!vhpi_validate_handle(handle, VHPI_TREE))
            return NULL;

         if (tree_kind(handle->tree) == T_ELAB)
            return (vhpiCharT *)"elaborated design";
         else if (class_has_type(class_of(handle->tree)))
            return (vhpiCharT *)type_pp(tree_type(handle->tree));
         else
            return (vhpiCharT *)tree_kind_str(tree_kind(handle->tree));
      }

   case vhpiToolVersionP:
      return (vhpiCharT *)PACKAGE_VERSION;

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
              vhpi_pretty_handle(handle));

   const vhpiPhysT invalid = { 0, 0 };

   if (property == vhpiResolutionLimitP)
      return vhpiFS;

   switch (property) {
   case vhpiPhysValP:
      {
         if (!vhpi_validate_handle(handle, VHPI_TREE))
            return invalid;

         if (!class_has_type(class_of(handle->tree))) {
            vhpi_error(vhpiError, tree_loc(handle->tree),
                       "object reference by handle does not have a type");
            return invalid;
         }

         type_t type = tree_type(handle->tree);
         if (!type_is_physical(type)) {
            vhpi_error(vhpiError, tree_loc(handle->tree),
                       "object does not have physical type");
            return invalid;
         }

         if (tree_kind(handle->tree) != T_SIGNAL_DECL) {
            vhpi_error(vhpiError, tree_loc(handle->tree), "vhpiPhysValP is "
                       "only supported for signal declaration objects");
            return invalid;
         }

         uint64_t value;
         rt_signal_value(handle->tree, &value, 1);

         const vhpiPhysT result = {
            .low  = value & 0xffffffff,
            .high = value >> 32
         };
         return result;
      }

   case vhpiPhysLeftBoundP:
   case vhpiPhysRightBoundP:
      {
         if (!vhpi_validate_handle(handle, VHPI_RANGE))
            return invalid;

         tree_t bound = property == vhpiPhysLeftBoundP
            ? handle->range.left : handle->range.right;

         if (!type_is_physical(tree_type(bound))) {
            vhpi_error(vhpiError, NULL, "type %s is not a physical type",
                       type_pp(tree_type(bound)));
            return invalid;
         }

         int64_t value;
         if (!folded_int(bound, &value))
            fatal_trace("physical type bound is not folded");

         const vhpiPhysT result = {
            .low  = value & 0xffffffff,
            .high = value >> 32
         };
         return result;
      }

   case vhpiPhysPositionP:
   case vhpiTimeP:
      fatal_trace("unsupported property %s in vhpi_get_phys",
                  vhpi_property_str(property));

   default:
      vhpi_error(vhpiError, tree_loc(handle->tree),
                 "invalid property %s in vhpi_get_phys",
                 vhpi_property_str(property));
   }

   return invalid;
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

   VHPI_TRACE("expr=%p value_p=%p", expr, value_p);

   if (!vhpi_validate_handle(expr, VHPI_TREE))
      return -1;

   if (tree_kind(expr->tree) != T_SIGNAL_DECL) {
      vhpi_error(vhpiInternal, tree_loc(expr->tree), "vhpi_get_value is only "
                 "supported for signal declaration objects");
      return -1;
   }

   type_t type = tree_type(expr->tree);
   type_t base = type_base_recur(type);

   ident_t type_name = type_ident(type);

   vhpiFormatT format;
   switch (type_kind(base)) {
   case T_ENUM:
      if ((type_name == std_logic_i) || (type_name == std_ulogic_i)
          || (type_name == std_bit_i)) {
         if (value_p->format == vhpiBinStrVal)
            format = value_p->format;
         else
            format = vhpiLogicVal;
      }
      else if (type_enum_literals(base) <= 256)
         format = vhpiSmallEnumVal;
      else
         format = vhpiEnumVal;
      break;

   case T_INTEGER:
      format = vhpiIntVal;
      break;

   case T_UARRAY:
   case T_CARRAY:
      {
         type_t elem = type_elem(base);
         switch (type_kind(elem)) {
         case T_ENUM:
            {
               ident_t elem_name = type_ident(elem);
               if ((elem_name == std_logic_i) || (elem_name == std_ulogic_i)
                   || (elem_name == std_bit_i)) {
                  if (value_p->format == vhpiBinStrVal)
                     format = value_p->format;
                  else
                     format = vhpiLogicVecVal;
               }
               else if (type_enum_literals(elem) <= 256)
                  format = vhpiSmallEnumVecVal;
               else
                  format = vhpiEnumVecVal;
               break;
            }

         default:
            vhpi_error(vhpiInternal, tree_loc(expr->tree), "arrays of type %s "
                       "not supported in vhpi_get_value", type_pp(elem));
            return -1;
         }
      }
      break;

   default:
      vhpi_error(vhpiInternal, tree_loc(expr->tree), "type %s not supported in "
                 "vhpi_get_value", type_pp(type));
      return -1;
   }

   if (value_p->format == vhpiObjTypeVal)
      value_p->format = format;
   else if (value_p->format != format) {
      vhpi_error(vhpiError, tree_loc(expr->tree), "invalid format %d for "
                 "object %s: expecting %d", value_p->format,
                 istr(tree_ident(expr->tree)), format);
      return -1;
   }

   if (format == vhpiBinStrVal) {
      const size_t need = rt_signal_string(expr->tree,
                                           vhpi_map_str_for_type(type),
                                           (char *)value_p->value.str,
                                           value_p->bufSize);
      if (need > value_p->bufSize)
         return need;
      else
         return 0;
   }
   else if (type_is_scalar(type)) {
      uint64_t value;
      rt_signal_value(expr->tree, &value, 1);

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
      value_p->numElems = rt_signal_value(expr->tree, values, max);

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
   // See LRM 2008 section 22.5.3 for discussion of semantics

   vhpi_clear_error();

   VHPI_TRACE("handle=%s value_p=%p mode=%d", vhpi_pretty_handle(handle),
              value_p, mode);

   if (!vhpi_validate_handle(handle, VHPI_TREE))
      return 1;

   bool propagate = false;
   switch (mode) {
   case vhpiForcePropagate:
      propagate = true;
   case vhpiForce:
      {
         type_t type = tree_type(handle->tree);
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

            case vhpiIntVal:
               expanded = value_p->value.intg;
               break;

            default:
               vhpi_error(vhpiFailure, tree_loc(handle->tree), "value format "
                          "%d not supported in vhpi_put_value",
                          value_p->format);
               return 1;
            }

            if (!propagate || rt_can_create_delta())
               rt_force_signal(handle->tree, &expanded, 1, propagate);
            else {
               vhpi_error(vhpiError, tree_loc(handle->tree), "cannot force "
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
               vhpi_error(vhpiFailure, tree_loc(handle->tree), " value format "
                          "%d not supported in vhpi_put_value",
                          value_p->format);
               return 1;
            }

            rt_force_signal(handle->tree, expanded, num_elems, propagate);
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

   VHPI_TRACE("vhpi_compare_handles handle1=%p handle2=%p", handle1, handle2);

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

int vhpi_release_handle(vhpiHandleT handle)
{
   vhpi_clear_error();

   VHPI_TRACE("handle=%s", vhpi_pretty_handle(handle));

   if (!vhpi_validate_handle(handle, VHPI_ANY))
      return 1;

   switch (handle->kind) {
   case VHPI_CALLBACK:
      switch (handle->cb.reason) {
      case vhpiCbStartOfSimulation:
      case vhpiCbEndOfSimulation:
      case vhpiCbEndOfProcesses:
      case vhpiCbNextTimeStep:
      case vhpiCbRepEndOfProcesses:
      case vhpiCbRepLastKnownDeltaCycle:
      case vhpiCbRepNextTimeStep:
      case vhpiCbLastKnownDeltaCycle:
         vhpi_forget_cb(&cb_list, handle);
         vhpi_free_obj(handle);
         return 0;

      case vhpiCbAfterDelay:
         if (handle->cb.list_pos != -1)
            vhpi_forget_cb(&cb_list, handle);

         if (handle->cb.fired)
            vhpi_free_obj(handle);
         else
            handle->cb.released = true;
         return 0;

      case vhpiCbValueChange:
         if (handle->cb.list_pos != -1)
            vhpi_forget_cb(&cb_list, handle);
         rt_set_event_cb(handle->tree, NULL, handle, false);
         return 0;

      default:
         assert(false);
      }

   case VHPI_TREE:
   case VHPI_TYPE:
   case VHPI_RANGE:
      assert(handle->refcnt > 0);
      if (--(handle->refcnt) == 0) {
         hash_put(handle_hash, handle->pointer, NULL);
         vhpi_free_obj(handle);
      }
      return 0;

   default:
      assert(false);
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
   top_level = top;

   if (handle_hash != NULL)
      hash_free(handle_hash);

   handle_hash = hash_new(1024, true);

   trace_on = opt_get_int("vhpi_trace_en");

   vhpi_clear_error();

   char *plugins_copy LOCAL = strdup(plugins);
   assert(plugins_copy);

   char *tok = strtok(plugins_copy, ",");
   do {
      notef("loading VHPI plugin %s", tok);

#ifdef __MINGW32__
      HMODULE hModule = LoadLibrary(tok);
      if (hModule == NULL)
         fatal_errno("failed to load %s", tok);

      void (**startup_funcs)() =
         (void (**)())GetProcAddress(hModule, "vhpi_startup_routines");

      if (startup_funcs == NULL) {
         warnf("could not find vhpi_startup_routines in %s", tok);
         CloseHandle(hModule);
         continue;
      }
#else
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
#endif

      while (*startup_funcs)
         (*startup_funcs++)();

   } while ((tok = strtok(NULL, ",")));

   atexit(vhpi_check_for_leaks);
}
