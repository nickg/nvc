//
//  Copyright (C) 2011-2015  Nick Gasson
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

#include "phase.h"
#include "util.h"
#include "common.h"
#include "rt/cover.h"

#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>

#define FUNC_REPLACE_MAX 32

typedef struct {
   tree_t   out;
   ident_t  path;    // Current 'PATH_NAME
   ident_t  inst;    // Current 'INSTANCE_NAME
   netid_t *next_net;
   lib_t    library;
   tree_t   arch;
} elab_ctx_t;

typedef struct {
   const tree_t *formals;
   const tree_t *actuals;
   int           count;
} rewrite_params_t;

typedef struct map_list {
   struct map_list *next;
   tree_t formal;
   tree_t actual;
   tree_t signal;
   tree_t name;
} map_list_t;

typedef struct copy_list copy_list_t;

struct copy_list {
   copy_list_t *next;
   tree_t       tree;
};

typedef struct {
   lib_t    lib;
   ident_t  name;
   tree_t  *tree;
} lib_search_params_t;

typedef struct generic_list generic_list_t;

struct generic_list {
   generic_list_t *next;
   ident_t         name;
   char           *value;
   bool            used;
};

static void elab_arch(tree_t t, const elab_ctx_t *ctx);
static void elab_block(tree_t t, const elab_ctx_t *ctx);
static void elab_stmts(tree_t t, const elab_ctx_t *ctx);
static void elab_funcs(tree_t arch, tree_t ent, const elab_ctx_t *ctx);
static void elab_copy_context(tree_t src, const elab_ctx_t *ctx);

static int errors = 0;

static generic_list_t *generic_override = NULL;

static ident_t hpathf(ident_t path, char sep, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf = xvasprintf(fmt, ap);
   va_end(ap);

   // LRM specifies instance path is lowercase
   char *p = buf;
   while (*p != '\0') {
      *p = tolower((int)*p);
      ++p;
   }

   ident_t id = ident_new(buf);
   free(buf);
   return ident_prefix(path, id, sep);
}

static const char *simple_name(const char *full)
{
   // Strip off any library or entity prefix from the parameter
   const char *start = full;
   for (const char *p = full; *p != '\0'; p++) {
      if (*p == '.' || *p == '-')
         start = p + 1;
   }

   return start;
}

static lib_t elab_find_lib(ident_t name, const elab_ctx_t *ctx)
{
   lib_t tmp = lib_work();
   lib_set_work(ctx->library);
   lib_t lib = lib_find(istr(ident_until(name, '.')), true, true);
   if (lib == NULL)
      fatal("cannot continue");
   lib_set_work(tmp);
   return lib;
}

static void find_arch(ident_t name, int kind, void *context)
{
   lib_search_params_t *params = context;

   ident_t prefix = ident_until(name, '-');

   if ((kind == T_ARCH) && (prefix == params->name)) {
      tree_t t = lib_get_check_stale(params->lib, name);
      assert(t != NULL);

      if (*(params->tree) == NULL)
         *(params->tree) = t;
      else {
         lib_mtime_t old_mtime = lib_mtime(params->lib,
                                           tree_ident(*(params->tree)));
         lib_mtime_t new_mtime = lib_mtime(params->lib, tree_ident(t));

         if (new_mtime == old_mtime) {
            // Analysed at the same time: compare line number
            // Note this assumes both architectures are from the same
            // file but this shouldn't be a problem with high-resolution
            // timestamps
            uint16_t new_line = tree_loc(t)->first_line;
            uint16_t old_line = tree_loc(*(params->tree))->first_line;

            if (new_line > old_line)
               *(params->tree) = t;
         }
         else if (new_mtime > old_mtime)
            *(params->tree) = t;
      }
   }
}

static tree_t pick_arch(const loc_t *loc, ident_t name, lib_t *new_lib,
                        const elab_ctx_t *ctx)
{
   // When an explicit architecture name is not given select the most
   // recently analysed architecture of this entity

   lib_t lib = elab_find_lib(name, ctx);

   tree_t arch = lib_get_check_stale(lib, name);
   if ((arch == NULL) || (tree_kind(arch) != T_ARCH)) {
      arch = NULL;
      lib_search_params_t params = { lib, name, &arch };
      lib_walk_index(lib, find_arch, &params);

      if (arch == NULL)
         fatal_at(loc, "no suitable architecture for %s", istr(name));
   }

   if (new_lib != NULL)
      *new_lib = lib;

   return arch;
}

static tree_t fixup_entity_refs(tree_t t, void *context)
{
   // Rewrite references to an entity to point at the selected architecture
   // so attributes like 'PATH_NAME are correct

   if (tree_kind(t) != T_REF)
      return t;

   tree_t arch = context;

   if (tree_ref(t) == tree_ref(arch))
      tree_set_ref(t, arch);

   return t;
}

static tree_t rewrite_refs(tree_t t, void *context)
{
   rewrite_params_t *params = context;

   if (tree_kind(t) != T_REF)
      return t;

   tree_t decl = tree_ref(t);

   for (int i = 0; i < params->count; i++) {
      if (decl != params->formals[i])
         continue;

      // Do not rewrite references if they appear as formal names
      if (tree_attr_int(t, formal_i, 0))
         continue;

      // Skip assignments to OPEN ports
      if (params->actuals[i] == NULL)
         continue;

      switch (tree_kind(params->actuals[i])) {
      case T_SIGNAL_DECL:
      case T_ENUM_LIT:
         tree_set_ref(t, params->actuals[i]);
         tree_set_type(t, tree_type(params->actuals[i]));
         return t;
      case T_LITERAL:
      case T_AGGREGATE:
      case T_REF:
      case T_ARRAY_SLICE:
      case T_ARRAY_REF:
      case T_FCALL:
      case T_CONCAT:
         return params->actuals[i];
      case T_TYPE_CONV:
         // XXX: this only works in trivial cases
         return tree_value(tree_param(params->actuals[i], 0));
      default:
         fatal_at(tree_loc(params->actuals[i]), "cannot handle tree kind %s "
                  "in rewrite_refs",
                  tree_kind_str(tree_kind(params->actuals[i])));
      }
   }

   return t;
}

static tree_t elab_port_to_signal(tree_t arch, tree_t port, tree_t actual)
{
   assert(tree_kind(port) == T_PORT_DECL);

   ident_t name = tree_ident(port);

   const int ndecls = tree_decls(arch);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(arch, i);
      if (tree_ident(d) == name)
         return d;
   }

   type_t port_type   = tree_type(port);
   type_t actual_type = tree_type(actual);

   type_t type = (type_is_unconstrained(port_type)) ? actual_type : port_type;

   port_mode_t mode = tree_subkind(port);

   tree_t s = tree_new(T_SIGNAL_DECL);
   tree_set_ident(s, tree_ident(port));
   tree_set_type(s, type);
   tree_add_attr_int(s, fst_dir_i, mode);

   if ((mode == PORT_OUT) || (mode == PORT_INOUT) || (mode == PORT_BUFFER)) {
      if (tree_has_value(port))
         tree_add_attr_tree(s, driver_init_i, tree_value(port));
   }

   tree_add_decl(arch, s);
   return s;
}

static void elab_add_context(tree_t t, const elab_ctx_t *ctx)
{
   ident_t cname = tree_ident(t);
   ident_t lname = ident_until(cname, '.');

   lib_t lib = elab_find_lib(lname, ctx);

   tree_t unit = lib_get(lib, cname);
   if (unit == NULL)
      fatal_at(tree_loc(t), "cannot find unit %s", istr(cname));
   else if (tree_kind(unit) == T_PACKAGE) {
      elab_copy_context(unit, ctx);

      ident_t name = tree_ident(unit);

      ident_t body_i = ident_prefix(name, ident_new("body"), '-');
      tree_t body = lib_get(lib, body_i);
      if (body != NULL)
         elab_copy_context(unit, ctx);
   }

   // Always use real library name rather than WORK alias
   tree_set_ident(t, tree_ident(unit));

   tree_add_context(ctx->out, t);
}

static bool elab_have_context(tree_t unit, ident_t name)
{
   const int ndest = tree_contexts(unit);
   for (int i = 0; i < ndest; i++) {
      tree_t c2 = tree_context(unit, i);
      if (tree_kind(c2) != T_USE)
         continue;

      if (tree_ident(c2) == name)
         return true;
   }

   return false;
}

static void elab_context_walk_fn(ident_t name, int kind, void *context)
{
   if (kind == T_PACKAGE) {
      const elab_ctx_t *ctx = (elab_ctx_t *)context;

      if (!elab_have_context(ctx->out, name)) {
         tree_t c = tree_new(T_USE);
         tree_set_ident(c, name);
         tree_set_ident2(c, all_i);

         elab_add_context(c, ctx);
      }
   }
}

static void elab_copy_context(tree_t src, const elab_ctx_t *ctx)
{
   const int nsrc = tree_contexts(src);
   for (int i = 0; i < nsrc; i++) {
      tree_t c = tree_context(src, i);
      if (tree_kind(c) != T_USE)
         continue;

      tree_set_ident2(c, all_i);

      ident_t name = tree_ident(c);
      ident_t lname = ident_until(name, '.');

      elab_ctx_t new_ctx = *ctx;
      new_ctx.library = elab_find_lib(lname, ctx);

      if (name == lname)
         lib_walk_index(new_ctx.library, elab_context_walk_fn, &new_ctx);
      else if (!elab_have_context(ctx->out, name))
         elab_add_context(c, &new_ctx);
   }
}

static void elab_pseudo_context(tree_t out, tree_t src)
{
   // Add a pseudo use clause for an entity or architecture so the
   // makefile generator can find the dependencies

   ident_t name = tree_ident(src);

   const int nctx = tree_contexts(out);
   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(out, i);
      if (tree_kind(c) != T_USE)
         continue;
      else if (tree_ident(c) == name)
         return;
   }

   tree_t c = tree_new(T_USE);
   tree_set_ident(c, name);

   tree_add_context(out, c);
}

static tree_t elab_signal_port(tree_t arch, tree_t formal, tree_t param,
                               map_list_t **maps)
{
   assert(tree_kind(param) == T_PARAM);

   tree_t actual = tree_value(param);

   // NULL name means associate the whole port
   tree_t name = NULL;
   if (tree_subkind(param) == P_NAMED) {
      tree_t n = tree_name(param);
      if (tree_kind(n) != T_REF)
         name = n;
   }

   const bool partial_map = (tree_kind(actual) != T_REF) || (name != NULL);

   switch (tree_kind(actual)) {
   case T_REF:
   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
   case T_RECORD_REF:
      {
         // Replace the formal port with a signal and connect its nets to
         // those of the actual

         tree_t ref = actual;
         tree_kind_t ref_kind;
         while ((ref_kind = tree_kind(ref)) != T_REF) {
            if ((ref_kind == T_AGGREGATE) || (ref_kind == T_LITERAL))
               return actual;
            else
               ref = tree_value(ref);
         }

         tree_t decl = tree_ref(ref);
         tree_kind_t decl_kind = tree_kind(decl);
         if (decl_kind == T_SIGNAL_DECL) {
            tree_t s = elab_port_to_signal(arch, formal, actual);

            if (partial_map)
               tree_add_attr_int(s, partial_map_i, 1);

            map_list_t *m = xmalloc(sizeof(map_list_t));
            m->next   = *maps;
            m->formal = formal;
            m->actual = actual;
            m->signal = s;
            m->name   = name;

            *maps = m;

            return s;
         }
         else if (decl_kind == T_PORT_DECL)
            return NULL;    // Port was OPEN at a higher level
         else
            return actual;
      }

   case T_LITERAL:
   case T_AGGREGATE:
      {
         type_t formal_type = tree_type(formal);
         if (!type_is_unconstrained(formal_type))
            tree_set_type(actual, formal_type);
         return actual;
      }

   case T_OPEN:
      return NULL;

   case T_TYPE_CONV:
      // Only allow simple array type conversions for now
      {
         type_t to_type   = tree_type(actual);
         type_t from_type = tree_type(tree_value(tree_param(actual, 0)));

         if (type_is_array(to_type) && type_is_array(from_type))
            return actual;
         else
            fatal_at(tree_loc(actual), "sorry, this form of type conversion "
                     "is not supported as an actual");
      }

   default:
      fatal_at(tree_loc(actual), "tree %s not supported as actual",
               tree_kind_str(tree_kind(actual)));
   }
}

static ident_t elab_formal_name(tree_t t)
{
   tree_kind_t kind;
   while ((kind = tree_kind(t)) != T_REF) {
      switch (kind) {
      case T_ARRAY_REF:
      case T_ARRAY_SLICE:
         t = tree_value(t);
         break;

      default:
         fatal_at(tree_loc(t), "sorry, this kind of formal is not supported %s",
                  tree_kind_str(kind));
      }
   }

   return tree_ident(t);
}

static map_list_t *elab_map(tree_t t, tree_t arch,
                            tree_formals_t tree_Fs, tree_formal_t tree_F,
                            tree_actuals_t tree_As, tree_actual_t tree_A)
{
   tree_t unit = tree_ref(arch);
   assert(tree_kind(unit) == T_ENTITY);

   const int nformals = tree_Fs(unit);
   const int nactuals = (tree_As != NULL) ? tree_As(t) : 0;

   bool *have_formals = xmalloc(sizeof(bool) * nformals);

   for (int i = 0; i < nformals; i++)
      have_formals[i] = false;

   const int maxr = nformals + nactuals;

   tree_t *rformals = xmalloc(sizeof(tree_t) * maxr);
   tree_t *ractuals = xmalloc(sizeof(tree_t) * maxr);
   int count = 0;

   map_list_t *maps = NULL;

   for (int i = 0; i < nactuals; i++) {
      tree_t p = tree_A(t, i);
      tree_t formal = NULL;

      switch (tree_subkind(p)) {
      case P_POS:
         {
            const int pos = tree_pos(p);
            formal = tree_F(unit, pos);
            have_formals[pos] = true;
         }
         break;
      case P_NAMED:
         {
            ident_t name = elab_formal_name(tree_name(p));
            for (int j = 0; j < nformals; j++) {
               tree_t port = tree_F(unit, j);
               if (tree_ident(port) == name) {
                  formal = port;
                  have_formals[j] = true;
                  break;
               }
            }
         }
         break;
      default:
         assert(false);
      }
      assert(formal != NULL);

      switch (tree_class(formal)) {
      case C_SIGNAL:
         ractuals[count] = elab_signal_port(arch, formal, p, &maps);
         break;

      case C_CONSTANT:
         ractuals[count] = tree_value(p);
         break;

      default:
         assert(false);
      }

      rformals[count] = formal;
      count++;
   }

   // Assign default values
   for (unsigned i = 0; i < nformals; i++) {
      if (!have_formals[i]) {
         tree_t f = tree_F(unit, i);
         if (tree_has_value(f)) {
            rformals[count] = f;
            ractuals[count] = tree_value(f);
            count++;
         }
      }
   }

   assert(count <= maxr);

   if (count > 0) {
      rewrite_params_t params = {
         .formals = rformals,
         .actuals = ractuals,
         .count   = count
      };
      tree_rewrite(arch, rewrite_refs, &params);

      tree_t ent = tree_ref(arch);
      if (tree_stmts(ent) > 0)
         tree_rewrite(ent, rewrite_refs, &params);
   }

   free(have_formals);
   free(rformals);
   free(ractuals);

   return maps;
}

static netid_t elab_get_net(tree_t expr, int n)
{
   switch (tree_kind(expr)) {
   case T_REF:
      return tree_net(tree_ref(expr), n);

   case T_ARRAY_REF:
      {
         tree_t value = tree_value(expr);
         type_t array_type = tree_type(value);

         const int nparams = tree_params(expr);

         int64_t offset = 0;
         for (int i = 0; i < nparams; i++) {
            tree_t index = tree_value(tree_param(expr, i));
            const int64_t dim_off =
               rebase_index(array_type, i, assume_int(index));

            if (i > 0) {
               range_t type_r = type_dim(array_type, i);
               int64_t low, high;
               range_bounds(type_r, &low, &high);

               offset *= high - low + 1;
            }

            offset += dim_off;
         }

         const int64_t stride = type_width(type_elem(array_type));
         return elab_get_net(value, n + offset * stride);
      }

   case T_ARRAY_SLICE:
      {
         tree_t value = tree_value(expr);
         type_t array_type = tree_type(value);

         range_t type_r  = type_dim(array_type, 0);
         range_t slice_r = tree_range(expr);

         assert(type_r.kind == slice_r.kind);

         const int64_t type_off =
            rebase_index(array_type, 0, assume_int(slice_r.left));

         const int stride = type_width(type_elem(array_type));

         return elab_get_net(value, n + (type_off * stride));
      }

   case T_RECORD_REF:
      {
         tree_t rec  = tree_value(expr);
         type_t type = tree_type(rec);

         const int roff = record_field_to_net(type, tree_ident(expr));

         return elab_get_net(rec, n + roff);
      }

   default:
      assert(false);
   }
}

static void elab_map_nets(map_list_t *maps)
{
   for (; maps != NULL; maps = maps->next) {
      if (maps->name == NULL) {
         // Associate the whole port
         const int awidth = type_width(tree_type(maps->actual));
         type_t ftype = tree_type(maps->signal);
         if (type_kind(ftype) != T_UARRAY) {
            const int fwidth = type_width(ftype);
            if (fwidth != awidth) {
               error_at(tree_loc(maps->actual), "actual width %d does not "
                        "match formal %s width %d", awidth,
                        istr(tree_ident(maps->signal)), fwidth);
               ++errors;
               continue;
            }
         }

         for (int i = 0; i < awidth; i++)
            tree_add_net(maps->signal, elab_get_net(maps->actual, i));
      }
      else {
         // Associate a sub-element or slice of the port
         switch (tree_kind(maps->name)) {
         case T_ARRAY_REF:
            {
               type_t array_type = tree_type(maps->formal);

               type_t elem_type = type_elem(array_type);
               const int width = type_width(elem_type);

               assert(tree_params(maps->name) == 1);

               tree_t index = tree_value(tree_param(maps->name, 0));
               const int64_t index_off =
                  rebase_index(array_type, 0, assume_int(index));

               for (int i = 0; i < width; i++)
                  tree_change_net(maps->signal, index_off * width + i,
                                  elab_get_net(maps->actual, i));
            }
            break;

         case T_ARRAY_SLICE:
            {
               type_t array_type = tree_type(maps->formal);

               range_t slice = tree_range(maps->name);

               int64_t low, high;
               range_bounds(slice, &low, &high);

               const int64_t base_off =
                  rebase_index(array_type, 0, assume_int(slice.left));

               const int width = MAX(high - low + 1, 0);
               for (int i = 0; i < width; i++)
                  tree_change_net(maps->signal, base_off + i,
                                  elab_get_net(maps->actual, i));
            }
            break;

         default:
            fatal_at(tree_loc(maps->formal), "sorry, tree kind %s not "
                     "supported as a formal",
                     tree_kind_str(tree_kind(maps->formal)));
         }
      }
   }
}

static bool elab_should_copy(tree_t t)
{
   switch (tree_kind(t)) {
   case T_SIGNAL_DECL:
   case T_GENVAR:
   case T_PROCESS:
   case T_ARCH:
      return true;
   case T_LITERAL:
   case T_ASSOC:
   case T_PARAM:
   case T_WAVEFORM:
   case T_ARRAY_SLICE:
   case T_UNIT_DECL:
   case T_USE:
   case T_IF_GENERATE:
   case T_CONCAT:
   case T_LIBRARY:
   case T_TYPE_CONV:
   case T_ALL:
   case T_OPEN:
   case T_ATTR_REF:
   case T_NEW:
   case T_BINDING:
   case T_SPEC:
       return false;
   case T_VAR_DECL:
      if (tree_attr_int(t, shared_i, 0))
         return true;
      // Fall-through
   default:
      return tree_attr_int(t, elab_copy_i, 0);
   }
}

static void elab_build_copy_list(tree_t t, void *context)
{
   copy_list_t **list = context;

   if (elab_should_copy(t)) {
      copy_list_t *new = xmalloc(sizeof(copy_list_t));
      new->tree = t;
      new->next = *list;

      *list = new;
   }
}

static bool elab_copy_trees(tree_t t, void *context)
{
   copy_list_t *list = context;

   if (elab_should_copy(t)) {
      for (; list != NULL; list = list->next) {
         if (list->tree == t)
            return true;
      }
   }

   return false;
}

static tree_t elab_copy(tree_t t)
{
   copy_list_t *copy_list = NULL;
   tree_visit(t, elab_build_copy_list, &copy_list);

   // For achitectures, also make a copy of the entity ports
   if (tree_kind(t) == T_ARCH)
      tree_visit(tree_ref(t), elab_build_copy_list, &copy_list);

   tree_t copy = tree_copy(t, elab_copy_trees, copy_list);

   while (copy_list != NULL) {
      copy_list_t *tmp = copy_list->next;
      free(copy_list);
      copy_list = tmp;
   }

   return copy;
}

static bool elab_compatible_map(tree_t comp, tree_t entity, char *what,
                                tree_formals_t tree_Fs, tree_formal_t tree_F)
{
   // TODO: for now they must exactly match up

   const int comp_nf   = (*tree_Fs)(comp);
   const int entity_nf = (*tree_Fs)(entity);

   for (int i = 0; i < comp_nf; i++) {
      tree_t comp_f = (*tree_F)(comp, i);

      bool found = false;
      for (int j = 0; j < entity_nf; j++) {
         tree_t entity_f = (*tree_F)(entity, j);

         if (tree_ident(comp_f) != tree_ident(entity_f))
            continue;

         found = true;

         type_t entity_type = tree_type(entity_f);
         type_t comp_type   = tree_type(comp_f);

         if (!type_eq(entity_type, comp_type)) {
            error_at(tree_loc(comp_f), "type of %s %s in component "
                     "declaration %s is %s which does not match type %s in "
                     "entity %s", what, istr(tree_ident(comp_f)),
                     istr(tree_ident(comp)), type_pp(comp_type),
                     type_pp(entity_type), istr(tree_ident(entity)));
            ++errors;
            return false;
         }
      }

      if (!found) {
         error_at(tree_loc(comp_f), "%s %s not found in entity %s",
                  what, istr(tree_ident(comp_f)), istr(tree_ident(entity)));
         ++errors;
         return false;
      }
   }

   return true;
}

static void find_entity(ident_t name, int kind, void *context)
{
   lib_search_params_t *params = context;

   if (kind == T_ENTITY) {
      if (params->name == name)
         *(params->tree) = lib_get_check_stale(params->lib, name);
   }
}

static tree_t elab_default_binding(tree_t inst, lib_t *new_lib,
                                   const elab_ctx_t *ctx)
{
   // Default binding indication is described in LRM 93 section 5.2.2

   tree_t comp = tree_ref(inst);

   ident_t full_i = tree_ident(comp);
   ident_t lib_i = ident_until(full_i, '.');

   lib_t lib = NULL;
   bool search_others = true;
   if (lib_i == full_i) {
      lib    = ctx->library;
      full_i = ident_prefix(lib_name(lib), full_i, '.');
   }
   else {
      search_others = false;
      lib = elab_find_lib(lib_i, ctx);

      // Strip out the component package name, if any
      full_i = ident_prefix(lib_i, ident_rfrom(full_i, '.'), '.');
   }

   tree_t entity = NULL;
   lib_search_params_t params = { lib, full_i, &entity };
   lib_walk_index(params.lib, find_entity, &params);

   if (entity == NULL) {
      if (search_others && ctx->arch != NULL) {
         tree_t elab_ent = tree_ref(ctx->arch);
         const int nctxe = tree_contexts(elab_ent);
         const int nctxa = tree_contexts(ctx->arch);
         for (int i = 0; entity == NULL && i < nctxe + nctxa; i++) {
            tree_t c = i < nctxe
               ? tree_context(elab_ent, i)
               : tree_context(ctx->arch, i - nctxe);

            if (tree_kind(c) != T_LIBRARY)
               continue;

            lib_t lib = elab_find_lib(tree_ident(c), ctx);
            full_i = ident_prefix(lib_name(lib), tree_ident(comp), '.');
            if (lib != NULL) {
               lib_search_params_t params = { lib, full_i, &entity };
               lib_walk_index(params.lib, find_entity, &params);
            }
         }
      }

      if (entity == NULL) {
         error_at(tree_loc(inst), "cannot find entity for component %s "
                  "without binding indication", istr(tree_ident(comp)));
         ++errors;
         return NULL;
      }
   }

   tree_t arch = elab_copy(pick_arch(tree_loc(comp),
                                     tree_ident(entity), new_lib, ctx));

   // Check entity is compatible with component declaration

   if (!elab_compatible_map(comp, entity, "generic",
                            tree_generics, tree_generic))
      return NULL;

   if (!elab_compatible_map(comp, entity, "port", tree_ports, tree_port))
      return NULL;

   return arch;
}

static void elab_instance(tree_t t, const elab_ctx_t *ctx)
{
   lib_t new_lib = NULL;
   tree_t arch = NULL;
   switch (tree_class(t)) {
   case C_ENTITY:
      arch = elab_copy(pick_arch(tree_loc(t), tree_ident2(t), &new_lib, ctx));
      break;

   case C_COMPONENT:
      arch = elab_default_binding(t, &new_lib, ctx);
      break;

   case C_CONFIGURATION:
      fatal_at(tree_loc(t), "sorry, configurations is not supported yet");
      break;

   default:
      assert(false);
   }

   if (arch == NULL)
      return;

   map_list_t *maps = elab_map(t, arch, tree_ports, tree_port,
                               tree_params, tree_param);

   (void)elab_map(t, arch, tree_generics, tree_generic,
                  tree_genmaps, tree_genmap);

   ident_t ninst = hpathf(ctx->inst, '@', "%s(%s)",
                          simple_name(istr(tree_ident2(arch))),
                          simple_name(istr(tree_ident(arch))));

   elab_ctx_t new_ctx = {
      .out      = ctx->out,
      .path     = ctx->path,
      .inst     = ninst,
      .next_net = ctx->next_net,
      .library  = new_lib,
      .arch     = arch
   };

   tree_t entity = tree_ref(arch);
   elab_copy_context(entity, &new_ctx);

   elab_funcs(arch, entity, ctx);
   simplify(arch);

   elab_map_nets(maps);

   while (maps != NULL) {
      map_list_t *tmp = maps->next;
      free(maps);
      maps = tmp;
   }

   elab_arch(arch, &new_ctx);
}

static void elab_signal_nets(tree_t decl, const elab_ctx_t *ctx)
{
   // Assign net IDs to each sub-element of a signal declaration

   if (tree_nets(decl) != 0) {
      // Nets have already been assigned e.g. from a port map
   }
   else {
      const int width = type_width(tree_type(decl));
      for (int i = 0; i < width; i++)
         tree_add_net(decl, (*ctx->next_net)++);
   }
}

static void elab_prot_body_decls(tree_t body)
{
   type_t type = tree_type(body);

   const int ntdecls = type_decls(type);
   for (int i = 0; i < ntdecls; i++) {
      tree_t d = type_decl(type, i);

      ident_t base = ident_rfrom(tree_ident(d), '.');
      tree_set_ident(d, ident_prefix(tree_ident(body), base, '.'));
   }

   const int ndecls = tree_decls(body);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(body, i);

      const tree_kind_t kind = tree_kind(d);
      if ((kind != T_FUNC_DECL) && (kind != T_FUNC_BODY)
          && (kind != T_PROC_DECL) && (kind != T_PROC_BODY))
         continue;

      tree_set_ident(d, ident_prefix(tree_ident(body),
                                     ident_rfrom(tree_ident(d), '.'), '.'));
   }
}

static void elab_decls(tree_t t, const elab_ctx_t *ctx)
{
   tree_add_attr_str(t, inst_name_i,
                     ident_prefix(ctx->inst, ident_new(":"), '\0'));

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      const char *label = simple_name(istr(tree_ident(d)));
      ident_t ninst = hpathf(ctx->inst, ':', "%s", label);
      ident_t npath = hpathf(ctx->path, ':', "%s", label);

      if (label[0] == ':')
         continue;  // Already named one instance of this

      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
         elab_signal_nets(d, ctx);
         // Fall-through
      case T_FUNC_BODY:
      case T_PROC_BODY:
      case T_ALIAS:
      case T_FILE_DECL:
      case T_VAR_DECL:
         tree_set_ident(d, npath);
         tree_add_decl(ctx->out, d);
         tree_add_attr_str(d, inst_name_i, ninst);
         break;
      case T_PROT_BODY:
         tree_set_ident(d, npath);
         elab_prot_body_decls(d);
         tree_add_decl(ctx->out, d);
         break;
      case T_FUNC_DECL:
      case T_PROC_DECL:
         tree_set_ident(d, npath);
         break;
      case T_CONST_DECL:
         tree_set_ident(d, npath);
         tree_add_attr_str(d, inst_name_i, ninst);
         tree_add_decl(ctx->out, d);
         break;
      default:
         break;
      }
   }
}

static void elab_push_scope(tree_t t, const elab_ctx_t *ctx)
{
   tree_t h = tree_new(T_HIER);
   tree_set_loc(h, tree_loc(t));
   tree_set_subkind(h, tree_kind(t));

   if (tree_kind(t) == T_PACKAGE) {
      const char *name = istr(tree_ident(t));
      char lower[strlen(name) + 1], *p;
      for (p = lower; *name != '\0'; p++, name++)
         *p = tolower((int)*name);
      *p = '\0';

      tree_set_ident(h, ident_new(lower));
   }
   else
      tree_set_ident(h, ident_new(strrchr(istr(ctx->path), ':') + 1));

   if ((tree_kind(t) == T_ARCH) && (tree_decls(ctx->out) > 0)) {
      // Convert an identifier like WORK.FOO-RTL to foo(rtl)
      const char *id = istr(tree_ident(t));
      while ((*id != '\0') && (*id++ != '.'))
         ;

      const size_t len = strlen(id);
      char str[len + 2];
      char *p = str;
      for (; *id != '\0'; p++, id++) {
         if (*id == '-')
            *p = '(';
         else
            *p = tolower((int)*id);
      }

      *p++ = ')';
      *p++ = '\0';

      tree_set_ident2(h, ident_new(str));
   }

   tree_add_decl(ctx->out, h);
}

static void elab_pop_scope(const elab_ctx_t *ctx)
{
   tree_t last_decl = tree_decl(ctx->out, tree_decls(ctx->out) - 1);
   tree_add_attr_int(last_decl, scope_pop_i,
                     tree_attr_int(last_decl, scope_pop_i, 0) + 1);
}

static void elab_for_generate(tree_t t, elab_ctx_t *ctx)
{
   int64_t low, high;
   range_bounds(tree_range(t), &low, &high);

   for (int64_t i = low; i <= high; i++) {
      tree_t copy = elab_copy(t);

      tree_t genvar = tree_ref(copy);

      tree_t formals[1] = { genvar };
      tree_t actuals[1] = { get_int_lit(genvar, i) };

      rewrite_params_t params = {
         .formals = formals,
         .actuals = actuals,
         .count   = 1
      };
      tree_rewrite(copy, rewrite_refs, &params);
      simplify(copy);

      ident_t npath = hpathf(ctx->path, '\0', "[%"PRIi64"]", i);
      ident_t ninst = hpathf(ctx->inst, '\0', "[%"PRIi64"]", i);

      elab_ctx_t new_ctx = {
         .out      = ctx->out,
         .path     = npath,
         .inst     = ninst,
         .next_net = ctx->next_net,
         .library  = ctx->library,
         .arch     = ctx->arch
      };

      elab_push_scope(copy, &new_ctx);
      elab_decls(copy, &new_ctx);
      elab_stmts(copy, &new_ctx);
      elab_pop_scope(&new_ctx);
   }
}

static void elab_if_generate(tree_t t, elab_ctx_t *ctx)
{
   const int64_t value = assume_int(tree_value(t));
   if (value != 0) {
      elab_decls(t, ctx);
      elab_stmts(t, ctx);
   }
}

static void elab_rename_subprograms(tree_t t, ident_t prefix)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      switch (tree_kind(d)) {
      case T_FUNC_DECL:
      case T_FUNC_BODY:
      case T_PROC_DECL:
      case T_PROC_BODY:
         {
            ident_t new = ident_prefix(prefix, tree_ident(d), '_');
            tree_set_ident(d, new);
            elab_rename_subprograms(d, new);
         }
         break;
      default:
         break;
      }
   }
}

static void elab_process(tree_t t, const elab_ctx_t *ctx)
{
   // Rename local functions in this process to avoid collisions in the
   // global namespace

   elab_rename_subprograms(t, ctx->path);

   tree_add_attr_str(t, inst_name_i,
                     ident_prefix(ctx->inst, ident_new(":"), '\0'));
}

static void elab_stmts(tree_t t, const elab_ctx_t *ctx)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      const char *label = istr(tree_ident(s));
      ident_t npath = hpathf(ctx->path, ':', "%s", label);
      ident_t ninst = hpathf(ctx->inst, ':', "%s", label);

      elab_ctx_t new_ctx = {
         .out      = ctx->out,
         .path     = npath,
         .inst     = ninst,
         .next_net = ctx->next_net,
         .library  = ctx->library,
         .arch     = ctx->arch
      };

      switch (tree_kind(s)) {
      case T_INSTANCE:
         elab_instance(s, &new_ctx);
         break;
      case T_BLOCK:
         elab_block(s, &new_ctx);
         break;
      case T_FOR_GENERATE:
         elab_for_generate(s, &new_ctx);
         break;
      case T_IF_GENERATE:
         elab_if_generate(s, &new_ctx);
         break;
      case T_PROCESS:
         elab_process(s, &new_ctx);
         // Fall-through
      default:
         tree_add_stmt(ctx->out, s);
      }

      tree_set_ident(s, npath);
   }
}

static void elab_block(tree_t t, const elab_ctx_t *ctx)
{
   elab_push_scope(t, ctx);
   elab_decls(t, ctx);
   elab_stmts(t, ctx);
   elab_pop_scope(ctx);
}

static void elab_arch(tree_t t, const elab_ctx_t *ctx)
{
   elab_stmts(tree_ref(t), ctx);
   elab_pseudo_context(ctx->out, t);
   elab_pseudo_context(ctx->out, tree_ref(t));
   elab_copy_context(t, ctx);
   elab_push_scope(t, ctx);
   elab_decls(t, ctx);
   elab_stmts(t, ctx);
   elab_pop_scope(ctx);

   tree_rewrite(t, fixup_entity_refs, t);

   tree_set_ident(t, ident_prefix(ctx->path, ident_new(":"), '\0'));
}

static void elab_top_level_ports(tree_t arch, const elab_ctx_t *ctx)
{
   tree_t ent = tree_ref(arch);

   const int nports = tree_ports(ent);
   if (nports == 0)
      return;

   // Replace top-level ports with signals that can be driven from VHPI

   tree_t rformals[nports], ractuals[nports];

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(ent, i);
      type_t type = tree_type(p);

      if (type_is_unconstrained(type))
         fatal_at(tree_loc(p), "port %s of top-level entity must not have "
                  "unconstrained array type", istr(tree_ident(p)));

      tree_t s = tree_new(T_SIGNAL_DECL);
      tree_set_ident(s, tree_ident(p));
      tree_set_loc(s, tree_loc(p));
      tree_set_type(s, type);
      tree_add_attr_int(s, fst_dir_i, tree_subkind(p));

      if (tree_has_value(p))
         tree_set_value(s, tree_value(p));
      else
         tree_set_value(s, make_default_value(type, tree_loc(p)));

      tree_add_decl(arch, s);

      rformals[i] = p;
      ractuals[i] = s;
   }

   rewrite_params_t params = {
      .formals = rformals,
      .actuals = ractuals,
      .count   = nports
   };

   tree_rewrite(arch, rewrite_refs, &params);

   if (tree_stmts(ent) > 0)
      tree_rewrite(ent, rewrite_refs, &params);
}

static tree_t elab_generic_parse(tree_t generic, const char *str)
{
   type_t type = tree_type(generic);

   if (type_is_array(type) && type_is_enum(type_elem(type)))
      return str_to_literal(str, NULL, type);

   int64_t value;
   if (!parse_value(type, str, &value))
      fatal("failed to parse \"%s\" as type %s for generic %s",
            str, type_pp(type), istr(tree_ident(generic)));

   if (type_is_enum(type)) {
      tree_t result = tree_new(T_REF);
      tree_set_type(result, type);
      tree_set_ident(result, ident_new(str));
      tree_set_ref(result, type_enum_literal(type, value));

      return result;
   }
   else if (type_is_integer(type)) {
      tree_t result = tree_new(T_LITERAL);
      tree_set_subkind(result, L_INT);
      tree_set_type(result, type);
      tree_set_ival(result, value);

      return result;
   }
   else
      fatal("cannot override generic %s of type %s", istr(tree_ident(generic)),
            type_pp(type));
}

static void elab_top_level_generics(tree_t arch, const elab_ctx_t *ctx)
{
   tree_t ent = tree_ref(arch);

   const int ngenerics = tree_generics(ent);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(ent, i);
      ident_t name = tree_ident(g);

      generic_list_t *it;
      for (it = generic_override;
           it != NULL && it->name != name;
           it = it->next)
         ;

      if (it != NULL) {
         tree_set_value(g, elab_generic_parse(g, it->value));
         it->used = true;
      }
      else if (!tree_has_value(g))
         fatal_at(tree_loc(g), "generic %s of top-level entity must have "
                  "default value or be specified using -gNAME=VALUE",
                  istr(name));
   }

   (void)elab_map(ent, arch, tree_generics, tree_generic, NULL, NULL);
}

static void elab_entity_arch(tree_t t, tree_t arch, const elab_ctx_t *ctx)
{
   const char *name = simple_name(istr(tree_ident(t)));
   ident_t ninst = hpathf(ctx->inst, ':', ":%s(%s)", name,
                          simple_name(istr(tree_ident(arch))));
   ident_t npath = hpathf(ctx->path, ':', ":%s", name);

   elab_top_level_ports(arch, ctx);
   elab_top_level_generics(arch, ctx);

   elab_pseudo_context(ctx->out, t);
   elab_copy_context(t, ctx);

   tree_add_attr_str(ctx->out, simple_name_i, npath);

   elab_funcs(arch, t, ctx);
   simplify(arch);

   elab_ctx_t new_ctx = {
      .out      = ctx->out,
      .path     = npath,
      .inst     = ninst,
      .next_net = ctx->next_net,
      .library  = ctx->library,
      .arch     = arch
   };
   elab_arch(arch, &new_ctx);
}

static tree_t rewrite_funcs(tree_t t, void *context)
{
   if (tree_kind(t) != T_FCALL)
      return t;

   tree_t decl = tree_ref(t);

   ident_t name = tree_ident(decl);
   for (tree_t *rlist = context; *rlist != NULL; ++rlist) {
      const bool match =
         (tree_ident(*rlist) == name)
         && (type_eq(tree_type(*rlist), tree_type(decl)));

      if (match) {
         tree_set_ref(t, *rlist);
         break;
      }
   }

   return t;
}

static void elab_funcs(tree_t arch, tree_t ent, const elab_ctx_t *ctx)
{
   // Look through all the package bodies required by the context and
   // rewrite references to function declarations with references to
   // the function body.  This allows these functions to be folded by
   // the simplify phase

   int nreplace = 0;
   tree_t rlist[FUNC_REPLACE_MAX];

   const int ncontext_arch = tree_contexts(arch);
   const int ncontext_ent  = tree_contexts(ent);
   for (int i = 0; i < ncontext_arch + ncontext_ent; i++) {
      tree_t c = (i < ncontext_arch)
         ? tree_context(arch, i)
         : tree_context(ent, i - ncontext_arch);

      if (tree_kind(c) != T_USE)
         continue;

      ident_t name = tree_ident(c);
      lib_t lib = elab_find_lib(name, ctx);

      ident_t body_i = ident_prefix(name, ident_new("body"), '-');
      tree_t body = lib_get_check_stale(lib, body_i);
      if (body == NULL)
         continue;

      const int ndecls = tree_decls(body);
      for (int j = 0; j < ndecls; j++) {
         tree_t decl = tree_decl(body, j);
         if (tree_kind(decl) != T_FUNC_BODY)
            continue;

         if (nreplace + 1 == FUNC_REPLACE_MAX) {
            tree_rewrite(arch, rewrite_funcs, rlist);
            nreplace = 0;
         }

         rlist[nreplace++] = decl;
         rlist[nreplace] = NULL;
      }
   }

   if (nreplace > 0)
      tree_rewrite(arch, rewrite_funcs, rlist);
}

static tree_t rewrite_package_signals(tree_t t, void *ctx)
{
   tree_t decl = ctx;

   if (tree_kind(t) != T_REF)
      return t;

   tree_t ref = tree_ref(t);

   if (tree_kind(ref) != T_SIGNAL_DECL)
      return t;

   if (tree_ident(ref) != tree_ident(decl))
      return t;

   tree_set_ref(t, decl);
   return t;
}

static void elab_package_signals(tree_t unit, const elab_ctx_t *ctx)
{
   bool have_signals = false;
   const int ndecls = tree_decls(unit);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(unit, i);
      if (tree_kind(d) != T_SIGNAL_DECL)
         continue;
      else if (tree_nets(d) > 0)
         continue;

      if (!have_signals) {
         elab_push_scope(unit, ctx);
         have_signals = true;
      }

      elab_signal_nets(d, ctx);

      tree_rewrite(ctx->out, rewrite_package_signals, d);

      tree_add_decl(ctx->out, d);

      ident_t i = ident_new(package_signal_path_name(tree_ident(d)));
      tree_set_ident(d, i);
      tree_add_attr_str(d, inst_name_i, i);
   }

   if (have_signals)
      elab_pop_scope(ctx);
}

static void elab_context_signals(const elab_ctx_t *ctx)
{
   const int nctx = tree_contexts(ctx->out);
   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(ctx->out, i);
      if (tree_kind(c) != T_USE)
         continue;

      ident_t name = tree_ident(c);
      lib_t lib = elab_find_lib(name, ctx);

      tree_t pack = lib_get_check_stale(lib, name);
      if ((pack == NULL) || (tree_kind(pack) != T_PACKAGE))
         continue;

      elab_package_signals(pack, ctx);

      ident_t body_i = ident_prefix(name, ident_new("body"), '-');
      tree_t body = lib_get_check_stale(lib, body_i);
      if (body == NULL)
         continue;

      elab_package_signals(body, ctx);
   }
}

void elab_set_generic(const char *name, const char *value)
{
   ident_t id = ident_new(name);

   for (generic_list_t *it = generic_override; it != NULL; it = it->next) {
      if (it->name == id)
         fatal("generic %s already has value '%s'", name, it->value);
   }

   generic_list_t *new = xmalloc(sizeof(generic_list_t));
   new->name  = id;
   new->value = strdup(value);
   new->used  = false;
   new->next  = generic_override;

   generic_override = new;
}

tree_t elab(tree_t top)
{
   tree_t e = tree_new(T_ELAB);
   tree_set_ident(e, ident_prefix(tree_ident(top),
                                  ident_new("elab"), '.'));

   netid_t next_net = 0;
   elab_ctx_t ctx = {
      .out      = e,
      .path     = NULL,
      .inst     = NULL,
      .next_net = &next_net,
      .library  = lib_work()
   };

   switch (tree_kind(top)) {
   case T_ENTITY:
      {
         tree_t arch = pick_arch(NULL, tree_ident(top), NULL, &ctx);
         elab_entity_arch(top, arch, &ctx);
      }
      break;
   case T_ARCH:
      elab_entity_arch(tree_ref(top), top, &ctx);
      break;
   default:
      fatal("%s is not a suitable top-level unit", istr(tree_ident(top)));
   }

   elab_context_signals(&ctx);

   if (errors > 0)
      return NULL;

   tree_add_attr_int(e, ident_new("nnets"), next_net);

   if (opt_get_int("cover"))
      cover_tag(e);

   bounds_check(e);

   for (generic_list_t *it = generic_override; it != NULL; it = it->next) {
      if (!it->used)
         warnf("generic value for %s not used", istr(it->name));
   }

   if (bounds_errors() == 0) {
      lib_put(lib_work(), e);
      return e;
   }
   else
      return NULL;
}
