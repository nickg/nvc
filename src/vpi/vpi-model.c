//
//  Copyright (C) 2024  Nick Gasson
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
#include "diag.h"
#include "hash.h"
#include "ident.h"
#include "jit/jit-ffi.h"
#include "jit/jit.h"
#include "option.h"
#include "rt/model.h"
#include "rt/mspace.h"
#include "rt/structs.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-number.h"
#include "vpi/vpi-macros.h"
#include "vpi/vpi-model.h"
#include "vpi/vpi-priv.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
   PLI_INT32 type;
   vpiHandle handle;
   loc_t     loc;
} c_vpiObject;

typedef struct {
   unsigned      count;
   unsigned      limit;
   c_vpiObject **items;
} vpiObjectList;

typedef A(vpiHandle) vpiHandleList;

typedef void (*vpiLazyFn)(c_vpiObject *);

typedef struct {
   vpiLazyFn     fn;
   vpiObjectList list;
} vpiLazyList;

typedef struct {
   c_vpiObject      object;
   ident_t          name;
   s_vpi_systf_data systf;
} c_callback;

DEF_CLASS(callback, vpiCallback, object);

typedef struct {
   c_vpiObject  object;
   vlog_node_t  where;
   jit_handle_t handle;
   vpiLazyList  decls;
} c_abstractScope;

typedef struct {
   c_abstractScope  scope;
   tree_t           block;
   rt_scope_t      *rtscope;
} c_module;

DEF_CLASS(module, vpiModule, scope.object);

typedef struct {
   c_vpiObject      object;
   vpiObjectList    args;
   vlog_node_t      where;
   c_abstractScope *scope;
} c_tfCall;

typedef struct {
   c_tfCall    tfcall;
   c_callback *callback;
   vpiHandle   handle;
} c_sysTfCall;

typedef struct {
   c_sysTfCall systfcall;
} c_sysTaskCall;

DEF_CLASS(sysTaskCall, vpiSysTaskCall, systfcall.tfcall.object);

typedef struct {
   c_sysTfCall systfcall;
} c_sysFuncCall;

DEF_CLASS(sysFuncCall, vpiSysFuncCall, systfcall.tfcall.object);

typedef struct {
   c_vpiObject object;
   vlog_node_t where;
   unsigned    argpos;
} c_expr;

typedef struct {
   c_expr    expr;
   PLI_INT32 subtype;
   PLI_INT32 size;
} c_constant;

DEF_CLASS(constant, vpiConstant, expr.object);

typedef struct {
   c_expr    expr;
   PLI_INT32 subtype;
   unsigned  argslot;
} c_operation;

DEF_CLASS(operation, vpiOperation, expr.object);

typedef struct {
   c_vpiObject    object;
   vpiObjectList *list;
   uint32_t       pos;
} c_iterator;

DEF_CLASS(iterator, vpiIterator, object);

typedef struct {
   c_vpiObject      object;
   vlog_node_t      where;
   c_abstractScope *scope;
   UNSAFE_MPTR      mptr;
} c_abstractDecl;

typedef struct {
   c_abstractDecl decl;
} c_net;

DEF_CLASS(net, vpiNet, decl.object);

typedef struct {
   c_abstractDecl decl;
} c_reg;

DEF_CLASS(reg, vpiReg, decl.object);

#define HANDLE_BITS      (sizeof(vpiHandle) * 8)
#define HANDLE_MAX_INDEX ((UINT64_C(1) << (HANDLE_BITS / 2)) - 1)

typedef struct {
   c_vpiObject *obj;
   uint32_t     refcount;
   uint32_t     generation;
} handle_slot_t;

typedef struct _vpi_context {
   shash_t       *strtab;
   rt_model_t    *model;
   hash_t        *objcache;
   tree_t         top;
   jit_t         *jit;
   handle_slot_t *handles;
   unsigned       num_handles;
   unsigned       free_hint;
   vpiHandleList  systasks;
   vpiObjectList  syscalls;
   c_sysTfCall   *call;
   jit_scalar_t  *args;
   tlab_t        *tlab;
   text_buf_t    *valuestr;
   mem_pool_t    *pool;
   vpiObjectList  recycle;
} vpi_context_t;

static c_vpiObject *build_expr(vlog_node_t v, c_abstractScope *scope);
static void vpi_lazy_decls(c_vpiObject *obj);

static vpi_context_t *global_context = NULL;   // TODO: thread local

static inline vpi_context_t *vpi_context(void)
{
   assert(global_context != NULL);
   return global_context;
}

static handle_slot_t *decode_handle(vpi_context_t *c, vpiHandle handle)
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

static vpiHandle handle_for(c_vpiObject *obj)
{
   assert(obj != NULL);

   vpi_context_t *c = vpi_context();

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
      vpi_error(vpiSystem, NULL, "too many active handles");
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
   return (obj->handle = (vpiHandle)bits);
}

static void drop_handle(vpi_context_t *c, vpiHandle handle)
{
   handle_slot_t *slot = decode_handle(c, handle);
   if (slot == NULL)
      return;

   assert(slot->refcount > 0);
   if (--(slot->refcount) > 0)
      return;

   c_vpiObject *obj = slot->obj;
   slot->obj = NULL;
   slot->generation++;

   obj->handle = NULL;

   c->free_hint = slot - c->handles;

   switch (obj->type) {
   case vpiCallback:
   case vpiIterator:
      APUSH(c->recycle, obj);
      break;
   default:
      break;
   }
}

static inline c_vpiObject *from_handle(vpiHandle handle)
{
   handle_slot_t *slot = decode_handle(vpi_context(), handle);
   if (likely(slot != NULL))
      return slot->obj;

   vpi_error(vpiSystem, NULL, "invalid handle %p", handle);
   return NULL;
}

static c_tfCall *is_tfCall(c_vpiObject *obj)
{
   switch (obj->type) {
   case vpiSysTaskCall:
   case vpiSysFuncCall:
      return container_of(obj, c_tfCall, object);
   default:
      return NULL;
   }
}

static c_sysTfCall *is_sysTfCall(c_vpiObject *obj)
{
   switch (obj->type) {
   case vpiSysTaskCall:
   case vpiSysFuncCall:
      return container_of(obj, c_sysTfCall, tfcall.object);
   default:
      return NULL;
   }
}

static c_abstractDecl *is_abstractDecl(c_vpiObject *obj)
{
   switch (obj->type) {
   case vpiReg:
   case vpiNet:
      return container_of(obj, c_abstractDecl, object);
   default:
      return NULL;
   }
}

static c_abstractScope *is_abstractScope(c_vpiObject *obj)
{
   switch (obj->type) {
   case vpiModule:
      return container_of(obj, c_abstractScope, object);
   default:
      return NULL;
   }
}

static const char *handle_pp(vpiHandle handle)
{
   static __thread text_buf_t *tb = NULL;

   if (handle == NULL)
      return "NULL";

   if (tb == NULL)
      tb = tb_new();
   else
      tb_rewind(tb);

   tb_printf(tb, "%p:{", handle);

   handle_slot_t *slot = decode_handle(vpi_context(), handle);
   if (slot == NULL)
      tb_cat(tb, "INVALID");
   else {
      c_vpiObject *obj = slot->obj;
      tb_cat(tb, vpi_type_str(obj->type));
   }

   tb_append(tb, '}');

   return tb_get(tb);
}

static void *new_object(size_t size, PLI_INT32 type)
{
   assert(size >= sizeof(c_vpiObject));

   c_vpiObject *obj = pool_calloc(vpi_context()->pool, size);
   obj->type = type;
   obj->loc  = LOC_INVALID;

   return obj;
}

static void *recyle_object(size_t size, PLI_INT32 type)
{
   vpi_context_t *c = vpi_context();

   for (int i = 0; i < c->recycle.count; i++) {
      c_vpiObject *obj = c->recycle.items[i];
      if (obj->type == type) {
         for (int j = i; j < c->recycle.count - 1; j++)
            c->recycle.items[j] = c->recycle.items[j + 1];
         ATRIM(c->recycle, c->recycle.count - 1);

         memset(obj, '\0', size);
         obj->type = type;
         obj->loc  = LOC_INVALID;

         return obj;
      }
   }

   return new_object(size, type);
}

static void vpi_list_reserve(vpiObjectList *list, unsigned num)
{
   if (list->limit >= num)
      return;

   assert(list->count == 0);
   assert(list->items == NULL);

   mem_pool_t *mp = vpi_context()->pool;

   list->limit = num;
   list->items = pool_malloc_array(mp, num, sizeof(c_vpiObject *));
}

static inline void vpi_list_add(vpiObjectList *list, c_vpiObject *obj)
{
   assert(list->count < list->limit);
   list->items[list->count++] = obj;
}

static vpiObjectList *expand_lazy_list(c_vpiObject *obj, vpiLazyList *lazy)
{
   vpiLazyFn fn = lazy->fn;
   if (fn != NULL) {
      lazy->fn = NULL;   // Avoid infinite recursion
      (*fn)(obj);
   }

   return &(lazy->list);
}

static void init_expr(c_expr *expr, vlog_node_t v)
{
   expr->where = v;
}

static void init_tfCall(c_tfCall *call, vlog_node_t v, c_abstractScope *scope)
{
   call->where = v;
   call->scope = scope;

   const int nparams = vlog_params(v);
   vpi_list_reserve(&(call->args), nparams);

   for (int i = 0, argslot = 1; i < nparams; i++) {
      c_vpiObject *obj = build_expr(vlog_param(v, i), scope);
      vpi_list_add(&(call->args), obj);

      c_operation *op = is_operation(obj);
      if (op != NULL) {
         op->argslot = argslot;
         argslot += 3;
      }
   }
}

static void init_sysTfCall(c_sysTfCall *call, vlog_node_t v,
                           c_callback *callback, c_abstractScope *scope)
{
   init_tfCall(&call->tfcall, v, scope);
   call->callback = callback;

   vpi_context_t *c = vpi_context();
   APUSH(c->syscalls, &(call->tfcall.object));
}

static void init_abstractDecl(c_abstractDecl *decl, vlog_node_t v,
                              c_abstractScope *scope)
{
   decl->object.loc = *vlog_loc(v);
   decl->where = v;
   decl->scope = scope;
}

static void init_abstractScope(c_abstractScope *scope, vlog_node_t v)
{
   scope->object.loc = *vlog_loc(v);
   scope->where      = v;
   scope->handle     = JIT_HANDLE_INVALID;
   scope->decls.fn   = vpi_lazy_decls;
}

static void build_net(vlog_node_t v, c_abstractScope *scope)
{
   c_net *net = new_object(sizeof(c_net), vpiNet);
   init_abstractDecl(&(net->decl), v, scope);

   vpi_list_add(&scope->decls.list, &(net->decl.object));
}

static void build_reg(vlog_node_t v, c_abstractScope *scope)
{
   c_reg *reg = new_object(sizeof(c_reg), vpiNet);
   init_abstractDecl(&(reg->decl), v, scope);

   vpi_list_add(&scope->decls.list, &(reg->decl.object));
}

static c_constant *build_constant(vlog_node_t v)
{
   c_constant *con = new_object(sizeof(c_constant), vpiConstant);
   init_expr(&con->expr, v);

   switch (vlog_kind(v)) {
   case V_STRING:
      con->subtype = vpiStringConst;
      con->size = number_width(vlog_number(v));
      assert(con->size % 8 == 0);
      break;
   case V_NUMBER:
      con->subtype = vpiBinaryConst;
      con->size = number_width(vlog_number(v));
      break;
   default:
      should_not_reach_here();
   }

   return con;
}

static c_operation *build_operation(vlog_node_t v)
{
   c_operation *op = new_object(sizeof(c_operation), vpiOperation);
   init_expr(&op->expr, v);

   switch (vlog_kind(v)) {
   case V_EMPTY:
      op->subtype = vpiNullOp;
      break;
   default:
      break;
   }

   return op;
}

static c_vpiObject *build_expr(vlog_node_t v, c_abstractScope *scope)
{
   switch (vlog_kind(v)) {
   case V_STRING:
   case V_NUMBER:
      return &(build_constant(v)->expr.object);
   case V_REF:
      {
         vlog_node_t d = vlog_ref(v);

         vpiObjectList *list =
            expand_lazy_list(&(scope->object), &(scope->decls));
         for (int i = 0; i < list->count; i++) {
            c_abstractDecl *decl = is_abstractDecl(list->items[i]);
            assert(decl != NULL);

            if (decl->where == d)
               return &(decl->object);
         }

         fatal_trace("cannot find declaration for %s", istr(vlog_ident(d)));
      }
   case V_BINARY:
   case V_UNARY:
   case V_SYS_FCALL:
   case V_EMPTY:
      return &(build_operation(v)->expr.object);
   default:
      fatal_trace("cannot build VPI expr for node kind %s",
                  vlog_kind_str(vlog_kind(v)));
   }

   return NULL;
}

static c_sysTaskCall *build_sysTaskCall(vlog_node_t where, c_callback *callback,
                                        c_abstractScope *scope)
{
   c_sysTaskCall *call = new_object(sizeof(c_sysTaskCall), vpiSysTaskCall);
   init_sysTfCall(&call->systfcall, where, callback, scope);

   return call;
}

static c_sysFuncCall *build_sysFuncCall(vlog_node_t where, c_callback *callback,
                                        c_abstractScope *scope)
{
   c_sysFuncCall *call = new_object(sizeof(c_sysTaskCall), vpiSysFuncCall);
   init_sysTfCall(&call->systfcall, where, callback, scope);

   return call;
}

static bool init_iterator(c_iterator *it, PLI_INT32 type, c_vpiObject *obj)
{
   c_tfCall *call = is_tfCall(obj);
   if (call != NULL) {
      switch (type) {
      case vpiArgument:
         it->list = &(call->args);
         return true;
      default:
         return false;
      }
   }

   return false;
}

static c_module *build_module(vlog_node_t v, tree_t block, rt_scope_t *s)
{
   c_module *m = new_object(sizeof(c_module), vpiModule);
   init_abstractScope(&m->scope, v);
   m->block = block;
   m->rtscope = s;

   return m;
}

static c_module *cached_module(tree_t block, rt_scope_t *s)
{
   assert(tree_kind(block) == T_BLOCK);

   hash_t *cache = vpi_context()->objcache;
   c_module *m = hash_get(cache, block);
   if (m == NULL) {
      tree_t hier = tree_decl(block, 0);
      assert(tree_kind(hier) == T_HIER);

      tree_t wrap = tree_ref(hier);
      assert(tree_kind(wrap) == T_VERILOG);

      vlog_node_t mod = tree_vlog(wrap);
      assert(vlog_kind(mod) == V_MODULE);

      m = build_module(mod, block, s);
      hash_put(cache, block, m);
   }

   return m;
}

static jit_t *vpi_get_jit(vpi_context_t *c)
{
   if (c->jit != NULL)
      return c->jit;

   return jit_for_thread();
}

static void *vpi_get_ptr(c_abstractDecl *decl)
{
   if (decl->mptr != NULL)
      return decl->mptr;

   jit_t *jit = vpi_get_jit(vpi_context());

   if (decl->scope->handle == JIT_HANDLE_INVALID) {
      c_module *mod = is_module(&(decl->scope->object));
      if (mod != NULL)
         decl->scope->handle = jit_lazy_compile(jit, mod->rtscope->name);
   }

   ident_t name = vlog_ident(decl->where);

   if (decl->scope->handle == JIT_HANDLE_INVALID)
      fatal_at(&(decl->object.loc), "cannot get pointer to %s", istr(name));

   return (decl->mptr = jit_get_frame_var(jit, decl->scope->handle, name));
}

static void vpi_lazy_decls(c_vpiObject *obj)
{
   c_abstractScope *s = is_abstractScope(obj);
   assert(s != NULL);

   const int ndecls = vlog_decls(s->where);

   vpi_list_reserve(&s->decls.list, ndecls);

   for (int i = 0; i < ndecls; i++) {
      vlog_node_t v = vlog_decl(s->where, i);
      switch (vlog_kind(v)) {
      case V_NET_DECL:
         build_net(v, s);
         break;
      case V_VAR_DECL:
         build_reg(v, s);
         break;
      default:
         break;
      }
   }
}

////////////////////////////////////////////////////////////////////////////////
// Public API

DLLEXPORT
vpiHandle vpi_register_systf(p_vpi_systf_data systf_data_p)
{
   vpi_clear_error();

   VPI_TRACE("tfname=%s", systf_data_p->tfname);

   assert(systf_data_p->tfname[0] == '$');  // TODO: add test

   c_callback *cb = recyle_object(sizeof(c_callback), vpiCallback);
   cb->systf = *systf_data_p;
   cb->name  = ident_new(systf_data_p->tfname);

   vpiHandle handle = handle_for(&cb->object);
   APUSH(vpi_context()->systasks, handle);

   return handle;
}

DLLEXPORT
PLI_INT32 vpi_release_handle(vpiHandle object)
{
   vpi_clear_error();

   VPI_TRACE("handle=%s", handle_pp(object));

   c_vpiObject *obj = from_handle(object);
   if (obj == NULL) {
      vpi_error(vpiError, NULL, "invalid handle %p", object);
      return 0;
   }

   drop_handle(vpi_context(), object);
   return 1;

}

DLLEXPORT
vpiHandle vpi_handle(PLI_INT32 type, vpiHandle refHandle)
{
   vpi_clear_error();

   VPI_TRACE("type=%s refHandle=%s", vpi_method_str(type),
             handle_pp(refHandle));

   vpi_context_t *c = vpi_context();

   if (refHandle == NULL) {
      switch (type) {
      case vpiSysTfCall:
         if (c->call != NULL)
            return handle_for(&c->call->tfcall.object);
         else
            return NULL;
      }
   }

   return NULL;
}

DLLEXPORT
vpiHandle vpi_iterate(PLI_INT32 type, vpiHandle refHandle)
{
   vpi_clear_error();

   VPI_TRACE("type=%s handle=%s", vpi_method_str(type), handle_pp(refHandle));

   c_vpiObject *obj = NULL;
   if (refHandle != NULL && (obj = from_handle(refHandle)) == NULL)
      return NULL;

   c_iterator *it = recyle_object(sizeof(c_iterator), vpiIterator);
   if (!init_iterator(it, type, obj)) {
      vpi_error(vpiError, obj ? &(obj->loc) : NULL,
                "relation %s not supported for handle %s",
                vpi_method_str(type), handle_pp(refHandle));
      return NULL;
   }

   return handle_for(&(it->object));
}

DLLEXPORT
vpiHandle vpi_scan(vpiHandle iterator)
{
   vpi_clear_error();

   VPI_TRACE("iterator=%s", handle_pp(iterator));

   c_vpiObject *obj = from_handle(iterator);
   if (obj == NULL)
      return NULL;

   c_iterator *it = cast_iterator(obj);
   if (it == NULL)
      return NULL;

   if (it->pos < it->list->count)
      return handle_for(it->list->items[it->pos++]);

   return NULL;
}

DLLEXPORT
PLI_INT32 vpi_get(PLI_INT32 property, vpiHandle object)
{
   vpi_clear_error();

   VPI_TRACE("property=%s object=%s", vpi_property_str(property),
             handle_pp(object));

   c_vpiObject *obj = from_handle(object);
   if (obj == NULL)
      return vpiUndefined;

   if (property == vpiType)
      return obj->type;

   c_constant *con = is_constant(obj);
   if (con != NULL) {
      switch (property) {
      case vpiConstType:
         return con->subtype;
      case vpiSize:
         return con->size;
      default:
         goto missing_property;
      }
   }

   c_operation *op = is_operation(obj);
   if (op != NULL) {
      switch (property) {
      case vpiSize:
         {
            vpi_context_t *c = vpi_context();
            if (c->args != NULL)
               return c->args[op->argslot].integer;

            goto missing_property;
         }
      }
   }

   c_abstractDecl *decl = is_abstractDecl(obj);
   if (decl != NULL) {
      switch (property) {
      case vpiSize:
         {
            sig_shared_t **ss = vpi_get_ptr(decl);
            rt_signal_t *s = container_of(*ss, rt_signal_t, shared);
            return signal_width(s);
         }
      }
   }

missing_property:
   vpi_error(vpiError, &(obj->loc), "object does not have property %s",
             vpi_property_str(property));
   return vpiUndefined;
}

DLLEXPORT
void vpi_get_value(vpiHandle handle, p_vpi_value value_p)
{
   vpi_clear_error();

   VPI_TRACE("handle=%s value_p=%p", handle_pp(handle), value_p);

   c_vpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return;

   vpi_context_t *c = vpi_context();
   tb_rewind(c->valuestr);

   c_constant *con = is_constant(obj);
   if (con != NULL) {
      switch (con->subtype) {
      case vpiStringConst:
         {
            number_t n = vlog_number(con->expr.where);
            for (int i = con->size/8 - 1; i >= 0; i--)
               tb_append(c->valuestr, number_byte(n, i));

            value_p->format = vpiStringVal;
            value_p->value.str = (PLI_BYTE8 *)tb_get(c->valuestr);
         }
         return;

      case vpiBinaryConst:
         {
            number_t n = vlog_number(con->expr.where);
            vpi_format_number(n, value_p->format, c->valuestr);

            value_p->value.str = (PLI_BYTE8 *)tb_get(c->valuestr);
            return;
         }
      }
   }

   c_operation *op = is_operation(obj);
   if (op != NULL && c->args != NULL) {
      int size = c->args[op->argslot].integer;
      assert(size <= 64);

      uint64_t abits = c->args[op->argslot + 1].integer;
      uint64_t bbits = c->args[op->argslot + 2].integer;

      vpi_format_number2(size, abits, bbits, value_p->format, c->valuestr);

      value_p->value.str = (PLI_BYTE8 *)tb_get(c->valuestr);
      return;
   }

   c_abstractDecl *decl = is_abstractDecl(obj);
   if (decl != NULL) {
      sig_shared_t **ss = vpi_get_ptr(decl);
      rt_signal_t *s = container_of(*ss, rt_signal_t, shared);
      number_t num = number_pack(signal_value(s), signal_width(s));

      vpi_format_number(num, value_p->format, c->valuestr);
      number_free(&num);

      value_p->value.str = (PLI_BYTE8 *)tb_get(c->valuestr);
      return;
   }

   vpi_error(vpiError, &(obj->loc), "cannot evaluate %s", handle_pp(handle));
}

DLLEXPORT
vpiHandle vpi_put_value(vpiHandle handle, p_vpi_value value_p,
                        p_vpi_time time_p, PLI_INT32 flags)
{
   vpi_clear_error();

   VPI_TRACE("handle=%s value_p=%p", handle_pp(handle), value_p);

   c_vpiObject *obj = from_handle(handle);
   if (obj == NULL)
      return NULL;

   vpi_context_t *c = vpi_context();

   c_sysFuncCall *fcall = is_sysFuncCall(obj);
   if (fcall != NULL && c->args != NULL) {
      assert(fcall->systfcall.callback->systf.sysfunctype == vpiTimeFunc);
      assert(value_p->format == vpiTimeVal);

      const p_vpi_time tm = value_p->value.time;
      c->args[0].integer = (uint64_t)tm->high << 32 | tm->low;
      c->args[1].integer = 0;

      return NULL;
   }

   vpi_error(vpiError, &(obj->loc), "cannot change value of %s",
             handle_pp(handle));
   return NULL;
}

vpi_context_t *vpi_context_new(void)
{
   assert(global_context == NULL);

   vpi_context_t *c = global_context = xcalloc(sizeof(vpi_context_t));
   c->objcache = hash_new(128);
   c->valuestr = tb_new();
   c->pool     = pool_new();

   vpi_register_builtins();

   return c;
}

void vpi_context_initialise(vpi_context_t *c, tree_t top, rt_model_t *model,
                            jit_t *jit, int argc, char **argv)
{
   assert(c->model == NULL);
   assert(c->top == NULL);
   assert(c->jit == NULL);

   c->model = model;
   c->top   = top;
   c->jit   = jit;
}

static void vpi_check_leaks(vpi_context_t *c)
{
   int nactive = 0;
   for (int i = 0; i < c->num_handles; i++) {
      if (c->handles[i].obj != NULL)
         nactive++;
   }

   if (nactive > 0) {
      diag_t *d = diag_new(DIAG_WARN, NULL);
      diag_printf(d, "VPI program exited with %d active handles", nactive);

      for (int i = 0; i < c->num_handles; i++) {
         handle_slot_t *slot = &(c->handles[i]);
         if (slot->obj != NULL)
            diag_printf(d, "\n%s with %d references",
                        handle_pp(slot->obj->handle), slot->refcount);
      }

      diag_emit(d);
   }
}

void vpi_context_free(vpi_context_t *c)
{
   if (opt_get_int(OPT_PLI_DEBUG)) {
      // Release all system call handles to prevent false positives
      for (int i = 0; i < c->syscalls.count; i++) {
         c_sysTfCall *call = is_sysTfCall(c->syscalls.items[i]);
         assert(call != NULL);

         if (call->handle != NULL)
            drop_handle(c, call->handle);
      }

      for (int i = 0; i < c->systasks.count; i++)
         drop_handle(c, c->systasks.items[i]);

      vpi_check_leaks(c);
   }

   assert(c == global_context);
   global_context = NULL;

   if (c->strtab != NULL)
      shash_free(c->strtab);

   ACLEAR(c->syscalls);
   ACLEAR(c->systasks);
   ACLEAR(c->recycle);

#ifdef DEBUG
   size_t alloc, npages;
   pool_stats(c->pool, &alloc, &npages);
   if (npages > 1)
      debugf("VPI allocated %zu bytes in %zu pages", alloc, npages);
#endif

   pool_free(c->pool);
   tb_free(c->valuestr);
   hash_free(c->objcache);
   free(c->handles);
   free(c);
}

////////////////////////////////////////////////////////////////////////////////
// Foreign function interface

vpiHandle vpi_bind_foreign(ident_t name, vlog_node_t where)
{
   vpi_context_t *c = vpi_context();

   c_vpiObject *obj = hash_get(c->objcache, where);
   if (obj != NULL) {
      c_sysTfCall *call = is_sysTfCall(obj);
      assert(call != NULL);
      return call->handle;
   }

   rt_model_t *m = get_model();
   rt_scope_t *scope = get_active_scope(m);
   c_module *mod = cached_module(scope->where, scope);

   for (int i = 0; i < c->systasks.count; i++) {
      c_vpiObject *obj = from_handle(c->systasks.items[i]);
      if (obj == NULL)
         continue;

      c_callback *cb = is_callback(obj);
      assert(cb != NULL);

      if (cb->name != name)
         continue;

      c_sysTfCall *call;
      if (cb->systf.type == vpiSysTask)
         call = &(build_sysTaskCall(where, cb, &(mod->scope))->systfcall);
      else
         call = &(build_sysFuncCall(where, cb, &(mod->scope))->systfcall);

      return (call->handle = handle_for(&(call->tfcall.object)));
   }

   return NULL;
}

void vpi_call_foreign(vpiHandle handle, jit_scalar_t *args, tlab_t *tlab)
{
   c_vpiObject *obj = from_handle(handle);
   if (obj == NULL)
      jit_msg(NULL, DIAG_FATAL, "called invalid system task");

   c_sysTfCall *call = is_sysTfCall(obj);
   assert(call != NULL);

   void *orig_p0 = args[0].pointer;

   vpi_context_t *c = vpi_context();
   assert(c->call == NULL);
   c->call = call;
   c->args = args;
   c->tlab = tlab;

   (*call->callback->systf.calltf)(call->callback->systf.user_data);

   assert(c->call == call);
   c->call = NULL;
   c->args = NULL;
   c->tlab = NULL;

   if (call->callback->systf.type == vpiSysFunc && args[0].pointer == orig_p0)
      jit_msg(NULL, DIAG_FATAL, "system function %s did not return a value",
              call->callback->systf.tfname);
}
