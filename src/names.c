//
//  Copyright (C) 2020-2022  Nick Gasson
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
#include "names.h"
#include "hash.h"
#include "phase.h"
#include "common.h"
#include "array.h"
#include "loc.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct scope scope_t;
typedef struct type_set type_set_t;
typedef struct _spec spec_t;

typedef enum {
   O_IDLE,
   O_POS,
   O_NAMED,
} overload_state_t;

typedef struct {
   ident_t           name;
   tree_t            tree;
   A(tree_t)         candidates;
   A(tree_t)         params;
   overload_state_t  state;
   nametab_t        *nametab;
   bool              error;
   bool              trace;
   type_t            signature;
   tree_t            prefix;
   unsigned          initial;
} overload_t;

struct _spec {
   spec_t      *next;
   spec_kind_t  kind;
   ident_t      ident;
   tree_t       tree;
   unsigned     matches;
};

struct scope {
   scope_t       *parent;
   hash_t        *members;
   hash_t        *gmap;
   spec_t        *specs;
   overload_t    *overload;
   ident_t        import;
   scope_t       *chain;
   formal_kind_t  formal_kind;
   tree_t         formal;
   ident_t        prefix;
   tree_t         container;
   bool           suppress;
};

struct nametab {
   scope_t    *top_scope;
   type_set_t *top_type_set;
   tree_t      std;
};

typedef enum {
   TS_CONDITION_CONVERSION = (1 << 0)
} type_set_flags_t;

struct type_set {
   A(type_t)         members;
   type_set_t       *down;
   type_set_flags_t  flags;
};

typedef enum {
   ITER_FREE, ITER_SELECTED
} iter_mode_t;

typedef struct {
   nametab_t   *tab;
   ident_t      name;
   unsigned     nth;
   scope_t     *where;
   scope_t     *limit;
   iter_mode_t  mode;
   ident_t      next;
   tree_t       prefix;
} iter_state_t;

static type_t _solve_types(nametab_t *tab, tree_t expr);
static void begin_iter(nametab_t *tab, ident_t name, iter_state_t *state);
static tree_t iter_name(iter_state_t *state);

static void begin_overload_resolution(overload_t *o);
static tree_t finish_overload_resolution(overload_t *o);

////////////////////////////////////////////////////////////////////////////////
// Type sets

static void type_set_push(nametab_t *tab)
{
   type_set_t *t = xcalloc(sizeof(type_set_t));
   t->down = tab->top_type_set;

   tab->top_type_set = t;
}

static void type_set_pop(nametab_t *tab)
{
   assert(tab->top_type_set != NULL);

   type_set_t *old = tab->top_type_set;
   tab->top_type_set = old->down;
   ACLEAR(old->members);
   free(old);
}

static text_buf_t *type_set_fmt(nametab_t *tab)
{
   text_buf_t *tb = tb_new();

   if (tab->top_type_set != NULL && tab->top_type_set->members.count > 0) {
      tb_printf(tb, " (");
      for (unsigned n = 0; n < tab->top_type_set->members.count; n++) {
         tb_printf(tb, "%s%s", n > 0 ? ", " : "",
                   type_pp(tab->top_type_set->members.items[n]));
      }
      tb_printf(tb, ")");
   }

   return tb;
}

static void type_set_add(nametab_t *tab, type_t t)
{
   assert(tab->top_type_set != NULL);

   if (t == NULL)
      return;

   assert(t != NULL);

   for (unsigned i = 0; i < tab->top_type_set->members.count; i++) {
      if (type_eq(tab->top_type_set->members.items[i], t))
         return;
   }

   APUSH(tab->top_type_set->members, t);
}

static bool type_set_restrict(nametab_t *tab, bool (*pred)(type_t))
{
   if (tab->top_type_set == NULL)
      return false;

   int j = 0;
   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      type_t type = tab->top_type_set->members.items[i];
      if ((*pred)(type))
         tab->top_type_set->members.items[j++] = type;
   }
   ATRIM(tab->top_type_set->members, j);

   return j > 0;
}

static bool type_set_uniq(nametab_t *tab, type_t *pt)
{
   assert(tab->top_type_set != NULL);

   if (tab->top_type_set->members.count == 1) {
      *pt = tab->top_type_set->members.items[0];
      return true;
   }
   else {
      *pt = NULL;
      return false;
   }
}

static bool type_set_error(nametab_t *tab)
{
   if (tab->top_type_set == NULL)
      return false;

   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      if (type_is_none(tab->top_type_set->members.items[i]))
         return true;
   }

   return false;
}

static bool type_set_contains(nametab_t *tab, type_t type)
{
   if (tab->top_type_set == NULL || tab->top_type_set->members.count == 0)
      return true;

   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      type_t member = tab->top_type_set->members.items[i];
      if (type_eq(type, member))
         return true;
      else if (type_is_none(member))
         continue;
      else if (type_is_universal(member) && type_is_convertible(member, type))
         return true;
      else if (type_is_convertible(type, member))
         return true;
   }

   return false;
}

////////////////////////////////////////////////////////////////////////////////
// Scopes

#if 0
static void dump_names(nametab_t *tab)
{
   int depth = 0;
   for (scope_t *s = tab->top_scope; s != NULL; s = s->parent, depth++) {
      printf("-- depth %d --\n", depth);

      hash_iter_t it = HASH_BEGIN;
      ident_t name;
      tree_t decl;
      while (hash_iter(s->members, &it, (const void **)&name, (void **)&decl)) {
         const loc_t *loc = tree_loc(decl);
         printf("%20s -> %-12s %-15s %s:%d\n", istr(name),
                tree_kind_str(tree_kind(decl)),
                class_has_type(class_of(decl)) ? type_pp(tree_type(decl)) : "",
                istr(loc_file(loc)), loc->first_line);
      }
   }
}
#endif

static tree_t scope_find(scope_t *s, ident_t name, scope_t *limit,
                         scope_t **where, int k)
{
   do {
      void *value = hash_get_nth(s->members, name, &k);
      if (value != NULL) {
         if (where != NULL)
            *where = s;
         return (tree_t)value;
      }
   } while (s != limit && (s = s->chain ?: s->parent));

   return NULL;
}

static scope_t *scope_containing(nametab_t *tab, tree_t decl)
{
   ident_t name = tree_ident(decl);
   scope_t *s = tab->top_scope;

   do {
      tree_t next;
      int k = 0, tmp;
      while (tmp = k++, (next = hash_get_nth(s->members, name, &tmp))) {
         if (next == decl)
            return s;
      }
   } while ((s = s->chain ?: s->parent));

   return NULL;
}

static bool can_overload(tree_t t)
{
   const tree_kind_t kind = tree_kind(t);
   if ((kind == T_ALIAS || kind == T_GENERIC_DECL) && tree_has_type(t)) {
      type_t type = tree_type(t);
      return type_is_subprogram(type) || type_is_none(type);
   }
   else
      return kind == T_ENUM_LIT
         || kind == T_UNIT_DECL
         || kind == T_FUNC_DECL
         || kind == T_FUNC_BODY
         || kind == T_PROC_DECL
         || kind == T_PROC_BODY
         || kind == T_ATTR_SPEC
         || kind == T_LIBRARY;  // Allow multiple redundant library declarations
}

nametab_t *nametab_new(void)
{
   nametab_t *tab = xcalloc(sizeof(nametab_t));
   return tab;
}

void nametab_finish(nametab_t *tab)
{
   assert(tab->top_scope == NULL);
   assert(tab->top_type_set == NULL);
   free(tab);
}

void push_scope(nametab_t *tab)
{
   scope_t *s = xcalloc(sizeof(scope_t));
   s->members = hash_new(128, false);  // XXX: allocate this on-demand
   s->parent  = tab->top_scope;
   s->prefix  = tab->top_scope ? tab->top_scope->prefix : NULL;

   tab->top_scope = s;
}

void pop_scope(nametab_t *tab)
{
   assert(tab->top_scope != NULL);
   scope_t *tmp = tab->top_scope->parent;
   hash_free(tab->top_scope->members);

   while (tab->top_scope->chain) {
      scope_t *tmp = tab->top_scope->chain->chain;
      hash_free(tab->top_scope->chain->members);
      free(tab->top_scope->chain);
      tab->top_scope->chain = tmp;
   }

   for (spec_t *it = tab->top_scope->specs, *next; it != NULL; it = next) {
      if (it->kind == SPEC_EXACT && it->matches == 0
          && !tab->top_scope->suppress)
         error_at(tree_loc(it->tree), "instance %s not found", istr(it->ident));

      next = it->next;
      free(it);
   }

   if (tab->top_scope->gmap != NULL)
      hash_free(tab->top_scope->gmap);

   free(tab->top_scope);
   tab->top_scope = tmp;
}

void suppress_errors(nametab_t *tab)
{
   tab->top_scope->suppress = true;
}

void map_generic_type(nametab_t *tab, type_t generic, type_t actual)
{
   assert(type_kind(generic) == T_GENERIC);

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128, true);

   hash_put(tab->top_scope->gmap, generic, actual);
}

void map_generic_package(nametab_t *tab, tree_t inst)
{
   assert(tree_kind(inst) == T_PACK_INST);

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128, true);

   tree_t pack = tree_ref(inst);

   const int ndecls = tree_decls(pack);
   for (int i = 0; i < ndecls; i++)
      hash_put(tab->top_scope->gmap, tree_decl(pack, i), tree_decl(inst, i));

   const int ngenerics = tree_generics(pack);
   for (int i = 0; i < ngenerics; i++)
      hash_put(tab->top_scope->gmap, tree_generic(pack, i),
               tree_generic(inst, i));
}

hash_t *get_generic_map(nametab_t *tab)
{
   return tab->top_scope->gmap;
}

type_t get_mapped_type(nametab_t *tab, type_t type)
{
   if (tab->top_scope->gmap != NULL)
      return hash_get(tab->top_scope->gmap, type) ?: type;
   else
      return type;
}

static scope_t *chain_scope(nametab_t *tab, ident_t tag)
{
   scope_t *s = xcalloc(sizeof(scope_t));
   s->members = hash_new(128, false);
   s->import  = tag;
   s->parent  = tab->top_scope->parent;

   scope_t **p;
   for (p = &(tab->top_scope->chain); *p; p = &((*p)->chain))
      ;
   return (*p = s);
}

static scope_t *scope_end_of_chain(scope_t *s)
{
   while (s && s->chain)
      s = s->chain;
   return s;
}

static scope_t *scope_formal_limit(nametab_t *tab)
{
   if (tab->top_scope->formal_kind == F_RECORD)
      return tab->top_scope;
   else
      return NULL;
}

static tree_t scope_find_enclosing(scope_t *s, scope_kind_t what)
{
   for (; s != NULL; s = s->parent) {
      if (s->container == NULL)
         continue;

      if (what == S_LOOP && is_loop_stmt(s->container))
         return s->container;
      else if (what == S_SUBPROGRAM && is_subprogram(s->container))
         return s->container;
      else if (what == S_DESIGN_UNIT && is_design_unit(s->container))
         return s->container;
      else if (what == S_PROCESS && tree_kind(s->container) == T_PROCESS)
         return s->container;
      else if (what == S_PROTECTED && tree_kind(s->container) == T_PROT_BODY)
         return s->container;
   }

   return NULL;
}

void scope_set_prefix(nametab_t *tab, ident_t prefix)
{
   tab->top_scope->prefix = ident_prefix(tab->top_scope->prefix, prefix, '.');
}

ident_t scope_prefix(nametab_t *tab)
{
   return tab->top_scope->prefix;
}

void scope_set_container(nametab_t *tab, tree_t container)
{
   tab->top_scope->container = container;
}

void scope_set_subprogram(nametab_t *tab, tree_t subprog)
{
   tab->top_scope->container = subprog;
   tab->top_scope->prefix    = tree_ident2(subprog);
}

tree_t find_enclosing(nametab_t *tab, scope_kind_t what)
{
   if (tab->top_scope)
      return scope_find_enclosing(tab->top_scope, what);
   else
      return NULL;
}

void scope_set_formal_kind(nametab_t *tab, tree_t formal, formal_kind_t kind)
{
   tab->top_scope->formal_kind = kind;
   tab->top_scope->formal = formal;

   if (formal != NULL) {
      switch (kind) {
      case F_GENERIC_MAP:
         insert_generics(tab, formal);
         break;
      case F_PORT_MAP:
         insert_ports(tab, formal);
         break;
      case F_RECORD:
         insert_field_names(tab, tree_type(formal));
         break;
      default:
         break;
      }
   }
}

formal_kind_t scope_formal_kind(nametab_t *tab)
{
   return tab->top_scope->formal_kind;
}

static void insert_name_at(scope_t *s, ident_t name, tree_t decl)
{
   hash_put(s->members, name, decl);

   if (decl != (tree_t)-1 && tree_kind(decl) == T_TYPE_DECL) {
      type_t type = tree_type(decl);
      switch (type_kind(type)) {
      case T_ENUM:
         {
            const int nlits = type_enum_literals(type);
            for (int i = 0; i < nlits; i++) {
               tree_t literal = type_enum_literal(type, i);
               ident_t lit_name = tree_ident(literal);

               insert_name_at(s, lit_name, literal);
            }
         }
         break;

      case T_PHYSICAL:
         {
            const int nunits = type_units(type);
            for (int i = 0; i < nunits; i++) {
               tree_t unit = type_unit(type, i);
               ident_t unit_name = tree_ident(unit);

               insert_name_at(s, unit_name, unit);
            }
         }
         break;

      default:
         break;
      }
   }
}

static bool is_forward_decl(nametab_t *tab, tree_t decl, tree_t existing)
{
   tree_kind_t tkind = tree_kind(decl);
   tree_kind_t ekind = tree_kind(existing);

   if (tkind == T_TYPE_DECL && ekind == T_TYPE_DECL)
      return type_kind(tree_type(existing)) == T_INCOMPLETE;
   else if ((tkind == T_FUNC_BODY && ekind == T_FUNC_DECL)
            || (tkind == T_PROC_BODY && ekind == T_PROC_DECL)) {
      return type_eq(tree_type(decl), tree_type(existing))
         && !(tree_flags(existing) & TREE_F_PREDEFINED);
   }
   else if (tkind == T_CONST_DECL && ekind == T_CONST_DECL)
      return tree_has_value(decl) && !tree_has_value(existing);
   else
      return false;
}

tree_t find_forward_decl(nametab_t *tab, tree_t decl)
{
   scope_t *region = tab->top_scope;
   if (region->container == decl)
      region = region->parent;

   ident_t name = tree_ident(decl);

   for (int n = 0; ; ) {
      scope_t *where;
      tree_t d = scope_find(region, name, region, &where, n++);
      if (d == (void *)-1)
         continue;  // Error marker
      else if (d == NULL)
         return NULL;
      else if (is_forward_decl(tab, decl, d))
         return d;
   }
}

void insert_name(nametab_t *tab, tree_t decl, ident_t alias, int depth)
{
#if 0
   printf("insert_name %s alias=%s decl=%s\n",
          istr(tree_ident(decl)), istr(alias), tree_kind_str(tree_kind(decl)));
#endif

   scope_t *s;
   for (s = tab->top_scope; depth > 0; depth--, s = s->parent)
      ;
   assert(s != NULL);

   const bool overload = can_overload(decl);
   tree_kind_t tkind = tree_kind(decl);

   ident_t name = alias ?: tree_ident(decl);

   // Do not insert subprograms that contain type errors
   if (is_subprogram(decl)) {
      type_t type = tree_type(decl);
      const int nparams = type_params(type);
      bool error = false;

      for (int i = 0; !error && i < nparams; i++) {
         if (type_is_none(type_param(type, i)))
            error = true;
      }

      if (type_kind(type) == T_FUNC && type_is_none(type_result(type)))
         error = true;

      if (error) {
         insert_name_at(s, name, (tree_t)-1);
         return;
      }
   }

   tree_t existing;
   int n = 0;
   do {
      scope_t *where;
      if ((existing = scope_find(s, name, s, &where, n++))) {
         if (existing == (void *)-1)
            continue;  // Error marker

         const tree_kind_t ekind = tree_kind(existing);
         if (ekind == T_UNIT_DECL || ekind == T_LIBRARY)
            continue;
         else if (ekind == T_TYPE_DECL
                  && type_is_protected(tree_type(existing))
                  && tkind == T_PROT_BODY)
            continue;
         else if (is_forward_decl(tab, decl, existing)) {
            // Replace forward declaration in same region with full definition
            continue;
         }

         if (tkind == T_ATTR_SPEC && ekind == T_ATTR_DECL) {
            // Ignore this
         }
         else if ((!overload || !can_overload(existing))
                  && s->overload == NULL) {
            error_at(tree_loc(decl), "%s already declared in this region",
                     istr(name));
            note_at(tree_loc(existing), "previous declaration of %s was here",
                    istr(name));
            return;
         }
         else if (tkind == T_ATTR_SPEC && ekind == T_ATTR_SPEC) {
            if (tree_ident(decl) == tree_ident(existing)
                && tree_ident2(decl) == tree_ident2(existing)
                && tree_class(decl) == tree_class(existing)) {
               error_at(tree_loc(decl), "attribute %s for %s %s already "
                        "declared in this region", istr(tree_ident(decl)),
                        class_str(tree_class(decl)), istr(tree_ident2(decl)));
               note_at(tree_loc(existing), "previous declaration was here");
               return;
            }
         }
         else if (overload && type_eq(tree_type(decl), tree_type(existing))) {
            if ((ekind == T_FUNC_BODY && tkind == T_FUNC_BODY)
                     || (ekind == T_PROC_BODY && tkind == T_PROC_BODY)) {
               error_at(tree_loc(decl), "duplicate subprogram body %s",
                        type_pp(tree_type(decl)));
               note_at(tree_loc(existing), "previous definition of %s was "
                       "here", type_pp(tree_type(existing)));
            }
            else if ((ekind == T_FUNC_DECL || ekind == T_PROC_DECL)
                     && (tree_flags(existing) & TREE_F_PREDEFINED)) {
               // Allow pre-defined operators be to hidden by
               // user-defined subprograms in the same region
               assert(!(tree_flags(decl) & TREE_F_PREDEFINED));
               tree_set_flag(existing, TREE_F_HIDDEN);
               continue;
            }
            else if (!type_is_none(tree_type(decl))) {
               error_at(tree_loc(decl), "%s already declared in this region",
                        type_pp(tree_type(decl)));
               note_at(tree_loc(existing), "previous declaration of %s was "
                       "here", type_pp(tree_type(existing)));
            }

            return;
         }
      }
   } while (existing != NULL);

   insert_name_at(s, name, decl);
}

void insert_spec(nametab_t *tab, tree_t spec, spec_kind_t kind,
                 ident_t ident, int depth)
{
   spec_t *s = xmalloc(sizeof(spec_t));
   s->next    = NULL;
   s->kind    = kind;
   s->ident   = ident;
   s->tree    = spec;
   s->matches = 0;

   scope_t *scope;
   for (scope = tab->top_scope; depth > 0; depth--, scope = scope->parent)
      ;
   assert(scope != NULL);

   spec_t **p;
   for (p = &(scope->specs); *p != NULL; p = &((*p)->next)) {
      if (kind == SPEC_EXACT && (*p)->ident == ident) {
         error_at(tree_loc(spec), "duplicate specification for instance %s",
                  istr(ident));
         note_at(tree_loc((*p)->tree), "previous specification was here");
         return;
      }
   }
   *p = s;
}

type_t resolve_type(nametab_t *tab, type_t incomplete)
{
   assert(type_kind(incomplete) == T_INCOMPLETE);

   ident_t name = ident_rfrom(type_ident(incomplete), '.');
   tree_t decl;
   int n = 0;
   while ((decl = scope_find(tab->top_scope, name, NULL, NULL, n++))) {
      if (decl == (void *)-1)
         continue;  // Error marker
      else if (tree_kind(decl) == T_TYPE_DECL) {
         type_t def = tree_type(decl);
         if (type_kind(def) != T_INCOMPLETE && type_eq(def, incomplete))
            return def;
      }
   }

   return incomplete;
}

bool name_is_formal(nametab_t *tab, ident_t id)
{
   if (tab->top_scope->formal_kind != F_SUBPROGRAM)
      return false;

   iter_state_t iter;
   ident_t subprog = tree_ident(tab->top_scope->formal);
   tree_t decl;
   begin_iter(tab, subprog, &iter);
   while ((decl = iter_name(&iter))
          && decl != (tree_t)-1 && is_subprogram(decl)) {
      const int nports = tree_ports(decl);
      for (int i = 0; i < nports; i++)
         if (tree_ident(tree_port(decl, i)) == id)
            return true;
   }

   return false;
}

tree_t query_name(nametab_t *tab, ident_t name)
{
   if (tab->top_scope->formal_kind == F_SUBPROGRAM)
      return NULL;

   scope_t *limit = scope_formal_limit(tab);
   tree_t decl = scope_find(tab->top_scope, name, limit, NULL, 0);

   if (decl == NULL || decl == (void *)-1)
      return NULL;
   else if (is_subprogram(decl)) {
      // Suprogram overload resolution is handled separately
      return NULL;
   }
   else if (can_overload(decl) && tree_kind(decl) != T_LIBRARY) {
      // Might need to disambiguate enum names later in solve_types
      if (scope_find(tab->top_scope, name, NULL, NULL, 1))
         return NULL;
   }

   return decl;
}

tree_t query_spec(nametab_t *tab, tree_t object)
{
   spec_t *others = NULL;
   for (spec_t *it = tab->top_scope->specs; it != NULL; it = it->next) {
      switch (it->kind) {
      case SPEC_ALL:
         if (tree_ident(tree_ref(object)) == tree_ident2(it->tree)) {
            it->matches++;
            return it->tree;
         }
         break;
      case SPEC_OTHERS:
         if (tree_ident(tree_ref(object)) == tree_ident2(it->tree))
            others = it;
         break;
      case SPEC_EXACT:
         if (it->ident == tree_ident(object)) {
            it->matches++;
            return it->tree;
         }
         break;
      }
   }

   if (others != NULL) {
      others->matches++;
      return others->tree;
   }

   return NULL;
}

static void begin_iter(nametab_t *tab, ident_t name, iter_state_t *state)
{
   state->tab    = tab;
   state->nth    = 0;
   state->where  = NULL;
   state->limit  = NULL;
   state->prefix = NULL;
   state->next   = ident_walk_selected(&name);
   state->mode   = name == NULL ? ITER_FREE : ITER_SELECTED;
   state->name   = name;
}

static tree_t iter_name(iter_state_t *state)
{
   if (state->mode == ITER_FREE)
      return scope_find(state->tab->top_scope, state->next, state->limit,
                        &(state->where), (state->nth)++);

   if (state->prefix == NULL) {
      do {
         if (state->prefix == NULL)
            state->prefix = scope_find(state->tab->top_scope, state->next,
                                       NULL, NULL, 0);
         else
            state->prefix = search_decls(state->prefix, state->next, 0);

         if (state->prefix == NULL) {
            assert(error_count() > 0);  // Parser should have reported
            return (tree_t)-1;
         }

         state->next = ident_walk_selected(&(state->name));
      } while (state->name != NULL);
   }

   tree_t d = search_decls(state->prefix, state->next, (state->nth)++);
   if (d == NULL && state->nth == 1) {
      assert(error_count() > 0);  // Parser should have reported
      return (tree_t)-1;
   }

   return d;
}

static int tree_stable_compar(const void *pa, const void *pb)
{
   tree_t a = *(tree_t *)pa;
   tree_t b = *(tree_t *)pb;

   if (a == NULL)
      return -1;
   else if (b == NULL)
      return 1;
   else
      return tree_loc(a)->first_line - tree_loc(b)->first_line;
}

tree_t resolve_name(nametab_t *tab, const loc_t *loc, ident_t name)
{
   iter_state_t iter;
   begin_iter(tab, name, &iter);
   iter.limit = scope_formal_limit(tab);

   // Skip over attribute specifications when looking for names as these
   // should never be resolved by normal references
   tree_t decl;
   tree_kind_t dkind = T_LAST_TREE_KIND;
   while ((decl = iter_name(&iter))
          && decl != (void *)-1
          && (dkind = tree_kind(decl)) == T_ATTR_SPEC)
      ;

   if (decl == (void *)-1) {
      // Supressed an earlier failure for this name
      return NULL;
   }
   else if (decl == NULL) {
      if (tab->top_scope->suppress) {
         // Suppressing cascading errors
         assert(error_count() > 0);
      }
      else if (name == ident_new("error")) {
         // Was a parse error, suppress further errors
      }
      else if (tab->top_scope->formal_kind != F_NONE) {
         if (tab->top_scope->formal == NULL)
            return NULL;  // Was earlier error

         tree_t unit = tab->top_scope->formal;

         switch (tab->top_scope->formal_kind) {
         case F_PORT_MAP:
            error_at(loc, "%s has no port named %s",
                     istr(tree_ident(unit)), istr(name));
            break;
         case F_GENERIC_MAP:
            error_at(loc, "%s has no generic named %s",
                     istr(tree_ident(unit)), istr(name));
            break;
         case F_SUBPROGRAM:
            fatal_trace("do not call resolve_name in subprogram formals");
            break;
         case F_RECORD:
            error_at(loc, "record type %s has no field named %s",
                     type_pp(tree_type(unit)), istr(name));
            break;
         case F_NONE:
            break;
         }

         if (tab->top_scope->formal_kind == F_PORT_MAP) {
            const int nports = tree_ports(unit);
            if (nports > 0) {
               LOCAL_TEXT_BUF tb = tb_new();
               tb_printf(tb, "%s %s has port%s ",
                         tree_kind(unit) == T_COMPONENT
                         ? "component" : "entity",
                         istr(tree_ident(unit)), nports > 1 ? "s" : "");
               for (int j = 0; j < nports; j++)
                  tb_printf(tb, "%s%s", j > 0 ? ", " : "",
                            istr(tree_ident(tree_port(unit, j))));
               note_at(tree_loc(unit), "%s", tb_get(tb));
            }
         }

         tab->top_scope->formal = NULL;  // Suppress further errors
      }
      else if (tab->top_scope->overload && !tab->top_scope->overload->error) {
         error_at(loc,"no possible overload of %s has formal %s",
                  istr(tab->top_scope->overload->name), istr(name));

         const unsigned count = tab->top_scope->overload->candidates.count;
         for (unsigned i = 0; i < count; i++) {
            tree_t t = tab->top_scope->overload->candidates.items[i];
            const int nports = tree_ports(t);
            if (nports > 0) {
               LOCAL_TEXT_BUF tb = tb_new();
               tb_printf(tb, "subprogram %s has argument%s ",
                         type_pp(tree_type(t)), nports > 1 ? "s" : "");
               for (int j = 0; j < nports; j++)
                  tb_printf(tb, "%s%s", j > 0 ? ", " : "",
                            istr(tree_ident(tree_port(t, j))));
               note_at(tree_loc(t), "%s", tb_get(tb));
            }
         }

         tab->top_scope->overload->error = true;
      }
      else if (tab->top_scope->overload == NULL) {
         error_at(loc, "no visible declaration for %s", istr(name));
      }

      // Suppress further errors for this name
      hash_put(tab->top_scope->members, name, (void *)-1);
   }
   else if (can_overload(decl) && dkind != T_LIBRARY) {
      // Might need to disambiguate enum names
      type_t uniq;
      tree_t decl1;
      if (tab->top_type_set != NULL && type_set_uniq(tab, &uniq)
          && type_eq(uniq, tree_type(decl)))
         ;   // Only choice for current type set
      else if ((decl1 = iter_name(&iter))) {
         SCOPED_A(tree_t) m = AINIT;
         APUSH(m, decl);

         // Suprogram overload resolution is handled separately so do
         // not generate an error if all the possible overloads are
         // subprograms
         bool only_subprograms = is_subprogram(decl);

         do {
            if (!can_overload(decl1))
                break;
            only_subprograms &= is_subprogram(decl1);
            APUSH(m, decl1);
         } while ((decl1 = iter_name(&iter)));

         if (tab->top_type_set) {
            unsigned wptr = 0;
            for (unsigned i = 0; i < m.count; i++) {
               if (class_has_type(class_of(m.items[i]))
                   && type_set_contains(tab, tree_type(m.items[i])))
                  m.items[wptr++] = m.items[i];
            }
            ATRIM(m, wptr);
         }

         if (m.count > 1 && !only_subprograms) {
            LOCAL_TEXT_BUF tb = tb_new();
            tree_kind_t what = T_LAST_TREE_KIND;
            for (unsigned i = 0; i < m.count; i++) {
               tb_printf(tb, "%s%s", i > 0 ? ", " : "",
                         type_pp(tree_type(m.items[i])));
               const tree_kind_t kind = tree_kind(m.items[i]);
               if (what == T_LAST_TREE_KIND)
                  what = kind;
               else if (what != kind)
                  what = T_REF;
            }

            error_at(loc, "ambiguous use of %s %s (%s)",
                     what == T_ENUM_LIT ? "enumeration literal"
                     : (what == T_UNIT_DECL ? "physical literal" : "name"),
                     istr(name), tb_get(tb));
            decl = NULL;

            if (tab->top_scope->overload)
               tab->top_scope->overload->error = true;

            // Sort the declarations to ensure order of errors is deterministic
            qsort(m.items, m.count, sizeof(tree_t), tree_stable_compar);

            for (unsigned i = 0; i < m.count; i++) {
               note_at(tree_loc(m.items[i]),
                       "visible declaration of %s as %s", istr(name),
                       type_pp(tree_type(m.items[i])));
            }
         }
         else if (m.count == 1)
            decl = m.items[0];
      }
   }
   else if (tab->top_scope->overload == NULL && dkind != T_ATTR_DECL
            && dkind != T_LIBRARY) {
      // Check for conflicting names imported from multiple packages
      scope_t *first = iter.where;
      iter.limit = scope_end_of_chain(first);
      tree_t conflict = iter_name(&iter);
      if (conflict == (tree_t)-1)
         ;   // Error marker
      else if (conflict != NULL && is_forward_decl(tab, decl, conflict))
         ;   // Forward declaration
      else if (conflict != NULL && is_forward_decl(tab, conflict, decl)) {
         // Forward declaration, prefer the full declaration
         decl = conflict;
      }
      else if (conflict != NULL && tree_kind(conflict) == T_PROT_BODY)
         ;
      else if (conflict != NULL && iter.where != NULL && iter.where->import
               && !first->import)
         ;   // Second declaration was potentially visible homograph
      else if (conflict != NULL && conflict != decl) {
         error_at(loc, "multiple conflicting visible declarations of %s",
                  istr(name));

         const struct { tree_t decl; scope_t *scope; } visible[2] = {
            { decl, first }, { conflict, iter.where }
         };

         for (int i = 0; i < 2; i++) {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_printf(tb, "visible declaration of %s", istr(name));
            if (visible[i].scope->import)
               tb_printf(tb, " imported from %s",
                         istr(visible[i].scope->import));

            note_at(tree_loc(visible[i].decl), "%s", tb_get(tb));
         }

         insert_name_at(tab->top_scope, name, (tree_t)-1);
      }
   }

   return decl;
}

static tree_t resolve_ref(nametab_t *tab, tree_t ref)
{
   type_t constraint;
   if (type_set_uniq(tab, &constraint) && type_is_subprogram(constraint)) {
      // Reference to subprogram or enumeration literal with signature

      iter_state_t iter;
      begin_iter(tab, tree_ident(ref), &iter);

      const bool allow_enum =
         type_kind(constraint) == T_FUNC && type_params(constraint) == 0;

      bool match = false;
      tree_t decl = NULL;
      while (!match && (decl = iter_name(&iter)) && decl != (tree_t)-1) {
         if (is_subprogram(decl)) {
            type_t signature = tree_type(decl);
            match = type_eq_map(constraint, signature, tab->top_scope->gmap);
         }
         else if (allow_enum && tree_kind(decl) == T_ENUM_LIT)
            match = type_eq(type_result(constraint), tree_type(decl));
      }

      if (!match) {
         const char *signature = strchr(type_pp(constraint), '[');
         error_at(tree_loc(ref), "no visible subprogram%s %s matches "
                  "signature %s", allow_enum ? " or enumeration literal" : "",
                  istr(tree_ident(ref)), signature);
         return NULL;
      }
      else
         return decl;
   }
   else {
      // Ordinary reference
      return resolve_name(tab, tree_loc(ref), tree_ident(ref));
   }
}

void resolve_resolution(nametab_t *tab, tree_t rname, type_t type)
{
   // Finding the resolution function is a special case of overload resolution

   if (tree_kind(rname) == T_AGGREGATE) {
      if (type_is_record(type))
         error_at(tree_loc(rname), "sorry, record element resolution is not "
                  "supported yet");
      else if (!type_is_array(type))
         error_at(tree_loc(rname), "non-composite type %s may not have element "
                  "resolution indication", type_pp(type));
      else if (tree_assocs(rname) != 1)
         error_at(tree_loc(rname), "non-record type %s may not have record "
                  "element resolution indication", type_pp(type));
      else {
         tree_t a0 = tree_value(tree_assoc(rname, 0));
         resolve_resolution(tab, a0, type_elem(type));
      }
   }
   else {
      assert(tree_kind(rname) == T_REF);

      type_set_push(tab);
      type_set_add(tab, type);

      overload_t o = {
         .tree     = rname,
         .state    = O_IDLE,
         .nametab  = tab,
         .trace    = false,
         .name     = tree_ident(rname)
      };
      begin_overload_resolution(&o);
      tree_set_ref(rname, finish_overload_resolution(&o));

      type_set_pop(tab);
   }
}

tree_t find_std(nametab_t *tab)
{
   if (tab->std == NULL) {
      tab->std = resolve_name(tab, NULL, ident_new("STANDARD"));
      if (tab->std == NULL)
         fatal_trace("cannot continue without STD.STANDARD");
   }

   return tab->std;
}

static bool already_imported(nametab_t *tab, ident_t tag)
{
   for (scope_t *s = tab->top_scope; s != NULL; s = s->parent) {
      for (scope_t *c = s->chain; c != NULL; c = c->chain) {
         if (c->import == tag)
            return true;
      }
   }

   return false;
}

static void insert_lib_unit(lib_t lib, ident_t name, int kind, void *context)
{
   nametab_t *tab = context;
   ident_t bare_name = ident_rfrom(name, '.');

   if (!already_imported(tab, name)) {
      scope_t *s = chain_scope(tab, name);
      insert_name_at(s, bare_name, lib_get(lib, name));
   }
}

void insert_names_from_use(nametab_t *tab, tree_t use)
{
   assert(tree_kind(use) == T_USE);

   if (!tree_has_ref(use))
      return;   // Was earlier error

   tree_t unit = tree_ref(use);
   ident_t unit_name = tree_ident(use);

   if (tree_kind(unit) == T_GENERIC_DECL) {
      assert(tree_class(unit) == C_PACKAGE);

      if ((unit = lib_get_qualified(type_ident(tree_type(unit)))) == NULL)
         return;

      assert(is_uninstantiated_package(unit));
   }

   const bool lib_import = tree_kind(unit) == T_LIBRARY;

   if (lib_import) {
      ident_t lib_name = tree_ident(unit);
      lib_t lib = lib_require(lib_name);
      if (lib_name == unit_name) {
         lib_walk_index(lib, insert_lib_unit, tab);
         return;
      }
      else if ((unit = lib_get_check_stale(lib, unit_name)) == NULL) {
         error_at(tree_loc(use), "unit %s not found in library %s",
                  istr(unit_name), istr(ident_until(unit_name, '.')));
         return;
      }
   }

   ident_t tag = unit_name;
   if (tree_has_ident2(use))
      tag = ident_prefix(tag, tree_ident2(use), '.');

   if (already_imported(tab, tag))
      return;

   if (tree_has_ident2(use)) {
      ident_t what = tree_ident2(use);
      scope_t *s = chain_scope(tab, tag);
      if (what == well_known(W_ALL)) {
         const int ndecls = tree_decls(unit);
         for (int i = 0; i < ndecls; i++) {
            tree_t d = tree_decl(unit, i);
            insert_name_at(s, tree_ident(d), d);
         }

         if (standard() >= STD_08) {
            const int ngenerics = tree_generics(unit);
            for (int i = 0; i < ngenerics; i++) {
               tree_t g = tree_generic(unit, i);
               insert_name_at(s, tree_ident(g), g);
            }
         }
      }
      else {
         int nth = 0;
         tree_t decl;
         while ((decl = search_decls(unit, what, nth++)))
            insert_name_at(s, tree_ident(decl), decl);

         if (nth == 1) {
            error_at(tree_loc(use), "object %s not found in unit %s",
                     istr(what), istr(tree_ident(unit)));
         }
      }
   }

   if (lib_import && (unit_name == tag || !already_imported(tab, unit_name))) {
      scope_t *s = chain_scope(tab, unit_name);
      ident_t bare_name = ident_rfrom(unit_name, '.');
      insert_name_at(s, bare_name, unit);
   }
}

void insert_decls(nametab_t *tab, tree_t container)
{
   int ndecls = tree_decls(container);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(container, i);
      if (tree_kind(d) == T_USE)
         insert_names_from_use(tab, d);
      else
         insert_name(tab, tree_decl(container, i), NULL, 0);
   }
}

void insert_ports(nametab_t *tab, tree_t container)
{
   const int nports = tree_ports(container);
   for (int i = 0; i < nports; i++)
      insert_name(tab, tree_port(container, i), NULL, 0);
}

void insert_generics(nametab_t *tab, tree_t container)
{
   const bool have_type_generics = standard() >= STD_08;

   const int ngenerics = tree_generics(container);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(container, i);
      if (have_type_generics && tree_class(g) == C_TYPE) {
         // Type generics are inserted eagerly
         ident_t id = tree_ident(g);
         if (scope_find(tab->top_scope, id, tab->top_scope, NULL, 0) == g)
            continue;
      }

      insert_name(tab, g, NULL, 0);
   }
}

void insert_names_from_context(nametab_t *tab, tree_t unit)
{
   const int ncontext = tree_contexts(unit);
   for (int i = 0; i < ncontext; i++) {
      tree_t c = tree_context(unit, i);
      switch (tree_kind(c)) {
      case T_LIBRARY:
         lib_require(tree_ident(c));
         insert_name(tab, c, NULL, 0);
         break;
      case T_USE:
         insert_names_from_use(tab, c);
         break;
      case T_CONTEXT_REF:
         if (tree_has_ref(c))
            insert_names_from_context(tab, tree_ref(c));
         break;
      default:
         fatal_trace("nametab cannot handle context kind %s",
                     tree_kind_str(tree_kind(c)));
      }
   }
}

void insert_field_names(nametab_t *tab, type_t record)
{
   assert(type_is_record(record));

   const int nfields = type_fields(record);
   for (int i = 0; i < nfields; i++)
      insert_name(tab, type_field(record, i), NULL, 0);
}

void insert_protected_decls(nametab_t *tab, type_t type)
{
   assert(type_is_protected(type));

   const int ndecls = type_decls(type);
   for (int i = 0; i < ndecls; i++)
      insert_name(tab, type_decl(type, i), NULL, 0);
}

void insert_names_for_config(nametab_t *tab, tree_t unit)
{
   switch (tree_kind(unit)) {
   case T_ARCH:
      insert_names_from_context(tab, tree_primary(unit));
      // Fall-through
   case T_ENTITY:
      insert_names_from_context(tab, unit);
      break;
   default:
      break;
   }

   const int ndecls = tree_decls(unit);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(unit, i);
      if (tree_kind(d) == T_COMPONENT)
         insert_name(tab, d, NULL, 0);
   }

   const int nstmts = tree_stmts(unit);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(unit, i);
      insert_name(tab, s, NULL, 0);
   }
}

////////////////////////////////////////////////////////////////////////////////
// Name mangling

void mangle_func(nametab_t *tab, tree_t decl)
{
   if (tree_has_ident2(decl))
      return;

   LOCAL_TEXT_BUF buf = tb_new();
   ident_t qual = ident_prefix(tab->top_scope->prefix, tree_ident(decl), '.');
   tb_printf(buf, "%s", istr(qual));

   const tree_kind_t kind = tree_kind(decl);
   const bool is_func = kind == T_FUNC_BODY || kind == T_FUNC_DECL;
   const int nports = tree_ports(decl);
   if (nports > 0 || is_func)
      tb_printf(buf, "(");

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(decl, i);
      if (tree_class(p) == C_SIGNAL)
         tb_printf(buf, "s");
      mangle_one_type(buf, tree_type(p));
   }

   if (nports > 0 || is_func)
      tb_printf(buf, ")");

   if (is_func)
      mangle_one_type(buf, type_result(tree_type(decl)));

   tree_set_ident2(decl, ident_new(tb_get(buf)));
}

void mangle_type(nametab_t *tab, type_t type)
{
   if (type_kind(type) == T_SUBTYPE && !type_has_ident(type))
      mangle_type(tab, type_base(type));
   else {
      ident_t id = ident_prefix(tab->top_scope->prefix, type_ident(type), '.');
      type_set_ident(type, id);
   }
}

void mangle_decl(nametab_t *tab, tree_t decl)
{
   ident_t id = ident_prefix(tab->top_scope->prefix, tree_ident(decl), '.');
   tree_set_ident2(decl, id);
}

////////////////////////////////////////////////////////////////////////////////
// Subprogram overload resolution

static void overload_trace_candidates(overload_t *o, const char *phase)
{
   if (o->trace) {
      printf("%s: %s\n", istr(o->name), phase);
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t d = o->candidates.items[i];
         printf("  %s%s\n", type_pp(tree_type(d)),
                (tree_flags(d) & TREE_F_PREDEFINED) ? " (predefined)" : "");
      }
   }
}

static void overload_prune_candidate(overload_t *o, int index)
{
   if (o->trace) {
      tree_t d = o->candidates.items[index];
      printf("%s: prune candidate %s%s\n", istr(o->name),
             type_pp(tree_type(o->candidates.items[index])),
             (tree_flags(d) & TREE_F_PREDEFINED) ? " (predefined)" : "");
   }

   o->candidates.items[index] = NULL;
}

static void overload_add_candidate(overload_t *o, tree_t d)
{
   tree_kind_t dkind = tree_kind(d);
   type_t dtype = tree_type(d);

   // Prefer subprogram bodies to declarations if both are visible

   tree_kind_t skind = T_LAST_TREE_KIND;
   bool prefer = false;
   switch (dkind) {
   case T_FUNC_BODY: skind = T_FUNC_DECL; prefer = true; break;
   case T_PROC_BODY: skind = T_PROC_DECL; prefer = true; break;
   case T_FUNC_DECL: skind = T_FUNC_BODY; break;
   case T_PROC_DECL: skind = T_PROC_BODY; break;
   default: break;
   }

   for (unsigned i = 0; i < o->candidates.count; i++) {
      tree_kind_t ikind = tree_kind(o->candidates.items[i]);
      if (ikind == skind && type_eq(tree_type(o->candidates.items[i]), dtype)) {
         if (prefer) o->candidates.items[i] = d;
         return;
      }
   }

   APUSH(o->candidates, d);
}

static void begin_overload_resolution(overload_t *o)
{
   if (o->prefix != NULL) {
      int nth = 0;
      tree_t d = NULL, container = o->prefix;
      if (tree_kind(container) == T_REF)
         container = tree_ref(container);
      while ((d = search_decls(container, o->name, nth++))) {
         if (is_subprogram(d))
            overload_add_candidate(o, d);
      }
   }
   else {
      unsigned unit_break = 0, scope_break = 0;
      bool saw_alias = false;
      scope_t *last = NULL;
      for (int k = 0; ; k++) {
         scope_t *where;
         tree_t next = scope_find(o->nametab->top_scope, o->name,
                                  NULL, &where, k);
         if (next == NULL || next == (tree_t)-1)
            break;

         if (tree_kind(next) == T_ALIAS) {
            tree_t value = tree_value(next);
            if (tree_kind(value) != T_REF || !tree_has_ref(value))
               continue;

            next = tree_ref(value);
            saw_alias = true;
         }

         if (!is_subprogram(next))
            continue;

         if (last && last->parent != where->parent)
            unit_break = o->candidates.count;

         if (last && last != where)
            scope_break = o->candidates.count;

         // LRM 93 section 10.3 on visibility specifies that if two
         // declarations are homographs then the one in the inner
         // scope hides the one in the outer scope. However
         // scope_find returns all matches based on identifier only,
         // but it does guarantee to return declarations in order
         // from innermost to outermost.
         type_t type = tree_type(next);
         bool homograph = false;
         for (unsigned i = 0; !homograph && i < unit_break; i++)
            homograph = type_eq(type, tree_type(o->candidates.items[i]));

         // Prune predefined operators which are hidden by user defined
         // operators in the same region.
         unsigned wptr = scope_break;
         for (unsigned i = scope_break; i < o->candidates.count; i++) {
            if (type_eq(type, tree_type(o->candidates.items[i]))) {
               if (tree_flags(o->candidates.items[i]) & TREE_F_PREDEFINED) {
                  overload_prune_candidate(o, i);
                  continue;
               }
               else if (tree_flags(next) & TREE_F_PREDEFINED)
                  homograph = true;
            }
            o->candidates.items[wptr++] = o->candidates.items[i];
         }
         ATRIM(o->candidates, wptr);

         if (!homograph)
            overload_add_candidate(o, next);

         last = where;
      }

      // Remove any duplicates from aliases
      if (o->candidates.count > 1 && saw_alias) {
         unsigned wptr = 0;
         for (unsigned i = 0; i < o->candidates.count; i++) {
            bool is_dup = false;
            for (unsigned j = 0; !is_dup && j < i; j++)
               is_dup = (o->candidates.items[i] == o->candidates.items[j]);

            if (!is_dup)
               o->candidates.items[wptr++] = o->candidates.items[i];
         }
         ATRIM(o->candidates, wptr);
      }
   }

   overload_trace_candidates(o, "initial candidates");

   if (o->trace && o->nametab->top_type_set->members.count > 0) {
      LOCAL_TEXT_BUF tb = type_set_fmt(o->nametab);
      printf("%s: context%s\n", istr(o->name), tb_get(tb));
   }

   o->initial = o->candidates.count;
   o->error   = type_set_error(o->nametab);

   if (o->initial == 0 && !o->error) {
      error_at(tree_loc(o->tree), "no visible subprogram declaration for %s",
               istr(o->name));
      o->error = true;
   }

   const bool allow_condition_conversion =
      !!(o->nametab->top_type_set->flags & TS_CONDITION_CONVERSION);

   if (o->candidates.count > 1 && !allow_condition_conversion) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         type_t type = tree_type(o->candidates.items[i]);
         if (type_kind(type) == T_FUNC
             && !type_set_contains(o->nametab, type_result(type)))
            overload_prune_candidate(o, i);
         else
            o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);
   }

   // Remove procedures in a function call context and functions in a
   // procedure call context
   if (o->candidates.count > 1) {
      const tree_kind_t kind = tree_kind(o->tree);
      const bool is_fcall = kind == T_FCALL || kind == T_PROT_FCALL;
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         type_kind_t typek = type_kind(tree_type(o->candidates.items[i]));
         if ((is_fcall && typek != T_FUNC) || (!is_fcall && typek != T_PROC))
            overload_prune_candidate(o, i);
         else
            o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);
   }

   if (o->candidates.count != o->initial)
      overload_trace_candidates(o, "after initial pruning");
}

static int param_compar(const void *pa, const void *pb)
{
   // Define a sort order for parameters
   tree_t a = *(tree_t *)pa;
   tree_t b = *(tree_t *)pb;

   if (tree_subkind(a) == P_POS) {
      if (tree_subkind(b) == P_POS)
         return tree_pos(a) - tree_pos(b);
      else
         return INT_MAX;
   }
   else
      return INT_MAX;
}

static tree_t finish_overload_resolution(overload_t *o)
{
   assert(o->state == O_IDLE);

   overload_trace_candidates(o, "before final pruning");

   // Prune candidates with more required arguments than supplied
   if (o->candidates.count > 1) {
      const int nactual = o->params.count;
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t d = o->candidates.items[i];
         int nrequired = 0;
         const int nports = tree_ports(d);
         for (int i = 0; i < nports; i++) {
            if (!tree_has_value(tree_port(d, i)))
               nrequired++;
         }

         if (nactual < nrequired)
            overload_prune_candidate(o, i);
         else
            o->candidates.items[wptr++] = d;
      }
      ATRIM(o->candidates, wptr);
   }

   // Allow explicitly defined operators to hide implicitly defined ones
   // in different scopes. This is required behaviour in VHDL-2008 (see
   // section 12.4) and an optional rule relaxation in earlier revisions.
   const bool prefer_explicit =
      standard() >= STD_08 || !!(relax_rules() & RELAX_PREFER_EXPLICT);

   if (o->candidates.count > 1 && prefer_explicit) {
      int nexplicit = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         if (!(tree_flags(o->candidates.items[i]) & TREE_F_PREDEFINED))
            nexplicit++;
      }

      if (nexplicit > 0) {
         unsigned wptr = 0;
         for (unsigned i = 0; i < o->candidates.count; i++) {
            if (tree_flags(o->candidates.items[i]) & TREE_F_PREDEFINED)
               overload_prune_candidate(o, i);
            else
               o->candidates.items[wptr++] = o->candidates.items[i];
         }
         ATRIM(o->candidates, wptr);
      }
   }

   // Prune candidates which do not have all the required arguments
   if (o->candidates.count > 1) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t d = o->candidates.items[i];
         const int nports = tree_ports(d);
         bool all_match = true;
         for (int j = 0; all_match && j < nports; j++) {
            tree_t port = tree_port(d, j);

            if (tree_has_value(port))
               continue;

            bool match = false;
            for (int k = 0; !match && k < o->params.count; k++) {
               tree_t param = o->params.items[k];
               switch (tree_subkind(param)) {
               case P_POS:
                  match = tree_pos(param) == j;
                  break;

               case P_NAMED:
                  {
                     tree_t ref = name_to_ref(tree_name(param));
                     if (ref != NULL)
                        match = tree_ident(ref) == tree_ident(port);
                  }
                  break;
               }
            }

            all_match = match;
         }

         if (!all_match)
            overload_prune_candidate(o, i);
         else
            o->candidates.items[wptr++] = d;
      }
      ATRIM(o->candidates, wptr);
   }

   overload_trace_candidates(o, "final candidates");

   int count = 0;
   tree_t result = NULL;
   for (unsigned i = 0; i < o->candidates.count; i++) {
      tree_t d = o->candidates.items[i];
      if (tree_flags(d) & TREE_F_UNIVERSAL) {
         // LRM 08 section 9.3.6 implicit conversions only occur when
         // there is a unique numeric type in the context If a univeral
         // operator is in the candidate list then the other candidates
         // can only be there because of implicit conversions
         result = o->candidates.items[i];
         count  = 1;
         break;
      }
      else {
         result = d;
         count++;
      }
   }

   if (count > 1 && !o->error) {
      error_at(tree_loc(o->tree), "ambiguous %s %s",
               ident_char(o->name, 0) == '"' ? "use of operator"
               : (tree_kind(o->tree) == T_FCALL ? "call to function"
                  : "call to procedure"),
               istr(o->name));

      for (unsigned i = 0; i < o->candidates.count; i++) {
         if (o->candidates.items[i])
            note_at(tree_loc(o->candidates.items[i]), "candidate %s",
                    type_pp(tree_type(o->candidates.items[i])));
      }
   }
   else if (count == 0 && !o->error) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "%s [", istr(o->name));
      qsort(o->params.items, o->params.count, sizeof(tree_t), param_compar);
      for (unsigned i = 0; i < o->params.count; i++)
         tb_printf(tb, "%s%s", i > 0 ? ", " : "",
                   type_pp(tree_type(tree_value(o->params.items[i]))));

      type_set_t *ts = o->nametab->top_type_set;
      if (ts != NULL && ts->members.count > 0) {
         tb_printf(tb, "%sreturn ", o->params.count > 0 ? " " : "");
         for (unsigned i = 0; i < ts->members.count; i++)
            tb_printf(tb, "%s%s", i > 0 ? " | " : "",
                      type_pp(ts->members.items[i]));
      }

      tb_append(tb, ']');

      error_at(tree_loc(o->tree), "no matching %s %s",
               ident_char(o->name, 0) == '"' ? "operator" : "subprogram",
               tb_get(tb));
   }

   ACLEAR(o->candidates);
   ACLEAR(o->params);

   return count == 1 ? result : NULL;
}

static void overload_push_names(overload_t *o)
{
   if (o == NULL)
      return;

   assert(o->state == O_IDLE);

   push_scope(o->nametab);
   o->nametab->top_scope->overload = o;

   for (unsigned i = 0; i < o->candidates.count; i++) {
      if (o->candidates.items[i]) {
         const int nports = tree_ports(o->candidates.items[i]);
         for (int j = 0; j < nports; j++)
            insert_name(o->nametab, tree_port(o->candidates.items[i], j),
                        NULL, 0);
      }
   }

   o->state = O_NAMED;
}

static void overload_positional_argument(overload_t *o, int pos)
{
   if (o == NULL)
      return;

   assert(o->state == O_IDLE);

   type_set_push(o->nametab);

   if (o->trace)
      printf("%s: positional argument %d\n", istr(o->name), pos);

   // Prune any overloads which do not have this argument position
   unsigned wptr = 0;
   for (unsigned i = 0; i < o->candidates.count; i++) {
      tree_t d = o->candidates.items[i];
      if (pos < tree_ports(d)) {
         tree_t p = tree_port(d, pos);
         type_set_add(o->nametab, tree_type(p));
         o->candidates.items[wptr++] = d;
      }
      else if (o->initial > 1)
         overload_prune_candidate(o, i);
      else
         o->candidates.items[wptr++] = d;
   }
   ATRIM(o->candidates, wptr);

   o->state = O_POS;
}

static void overload_named_argument(overload_t *o, tree_t name)
{
   if (o == NULL)
      return;

   assert(o->state == O_NAMED);

   pop_scope(o->nametab);
   type_set_push(o->nametab);

   if ((name = name_to_ref(name)) == NULL)
      return;

   ident_t ident = tree_ident(name);

   if (o->trace)
      printf("%s: named argument %s\n", istr(o->name), istr(ident));

   // Prune any overloads which do not have this named argument
   unsigned wptr = 0;
   for (unsigned i = 0; i < o->candidates.count; i++) {
      const int nports = tree_ports(o->candidates.items[i]);
      tree_t port = NULL;
      for (int j = 0; j < nports; j++) {
         tree_t p = tree_port(o->candidates.items[i], j);
         if (tree_ident(p) == ident) {
            type_set_add(o->nametab, tree_type(p));
            port = p;
            break;
         }
      }

      if (port == NULL && o->initial > 1)
         overload_prune_candidate(o, i);
      else {
         o->candidates.items[wptr++] = o->candidates.items[i];
      }
   }
   ATRIM(o->candidates, wptr);
}

static tree_t overload_find_port(tree_t decl, tree_t param)
{
   switch (tree_subkind(param)) {
   case P_POS:
      {
         unsigned pos = tree_pos(param);
         if (pos < tree_ports(decl))
            return tree_port(decl, pos);
         else
            return NULL;
      }
   case P_NAMED:
      {
         tree_t ref = name_to_ref(tree_name(param));
         if (ref != NULL) {
            ident_t name = tree_ident(ref);
            const int nports = tree_ports(decl);
            for (int j = 0; j < nports; j++) {
               tree_t port_j = tree_port(decl, j);
               if (tree_ident(port_j) == name)
                  return port_j;
            }
         }
      }
      break;
   }

   return NULL;
}

static void overload_next_argument(overload_t *o, tree_t p)
{
   if (o == NULL)
      return;

   assert(tree_kind(p) == T_PARAM);
   assert(o->state != O_IDLE);

   tree_t value = tree_value(p);
   type_t type = _solve_types(o->nametab, value);

   if (o->trace) {
      printf("%s: next argument %s %s\n", istr(o->name),
             tree_kind_str(tree_kind(value)), type_pp(type));
   }

   if (o->initial > 1) {
      unsigned wptr = 0;
      int first_match = -1;
      bool found_port = false;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t port = overload_find_port(o->candidates.items[i], p);
         if (port != NULL && type != NULL) {
            type_t ptype = tree_type(port);
            found_port = true;

            if (type_eq(ptype, type) || type_is_convertible(type, ptype)) {
               // We've found at least one candidate with a matching
               // argument. Prune all the previous candidates that were
               // speculatively allowed through.
               if (first_match == -1) {
                  assert(wptr == i);
                  for (unsigned j = 0; j < wptr; j++)
                     overload_prune_candidate(o, j);
                  wptr = 0;
                  first_match = i;
               }
            }
            else if (first_match != -1) {
               overload_prune_candidate(o, i);
               continue;
            }
         }

         o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);

      if (first_match == -1 && found_port) {
         // No candidates matched this positional argument. Delete all
         // those that were speculatively allowed through.
         for (unsigned i = 0; i < wptr; i++)
            overload_prune_candidate(o, i);
         ATRIM(o->candidates, 0);
      }
   }

   if (type != NULL && type_is_none(type))
      o->error = true;  // Suppress further errors

   APUSH(o->params, p);

   type_set_pop(o->nametab);

   o->state = O_IDLE;
}

static void overload_restrict_argument(overload_t *o, tree_t p,
                                       const type_t *types, unsigned ntypes)
{
   assert(tree_kind(p) == T_PARAM);
   assert(o->state == O_IDLE);

   if (o->trace) {
      printf("%s: restrict argument ", istr(o->name));
      if (tree_subkind(p) == P_POS)
         printf("%d to", tree_pos(p));
      else if (tree_kind(tree_name(p)) == T_REF)
         printf("%s to", istr(tree_ident(tree_name(p))));
      for (unsigned i = 0; i < ntypes; i++)
         printf(" %s", type_pp(types[i]));
      printf("\n");
   }

   if (o->initial > 1) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t port = overload_find_port(o->candidates.items[i], p);
         if (port != NULL) {
            type_t ptype = tree_type(port);

            int matches = 0;
            for (unsigned j = 0; matches == 0 && j < ntypes; j++) {
               if (type_eq(ptype, types[j])
                   || type_is_convertible(types[j], ptype))
                  matches++;
            }

            if (matches == 0 && tree_subkind(p) == P_POS) {
               overload_prune_candidate(o, i);
               continue;
            }
         }

         o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);
   }
}

void map_generic_box(nametab_t *tab, tree_t inst, tree_t g)
{
   // Find the actual for the <> "box" default generic subprogram

   assert(tree_kind(g) == T_GENERIC_DECL);
   type_t type = tree_type(g);

   tree_t ref = tree_new(T_REF);
   tree_set_loc(ref, tree_loc(inst));
   tree_set_ident(ref, type_ident(type));
   tree_set_type(ref, solve_types(tab, ref, type));

   tree_t map = tree_new(T_PARAM);
   tree_set_loc(ref, tree_loc(inst));
   tree_set_subkind(map,  P_NAMED);
   tree_set_name(map, make_ref(g));
   tree_set_value(map, ref);

   tree_add_genmap(inst, map);
}

////////////////////////////////////////////////////////////////////////////////
// Scope checks
//
// These checks really belong in sem.c but they require information
// about the surrounding scope which is not preserved.

static void check_pure_ref(nametab_t *tab, tree_t ref, tree_t decl)
{
   tree_t sub = find_enclosing(tab, S_SUBPROGRAM);

   const bool is_pure_func =
      sub != NULL
      && tree_kind(sub) == T_FUNC_BODY
      && !(tree_flags(sub) & TREE_F_IMPURE);

   class_t class = class_of(decl);
   const bool maybe_impure =
      class == C_VARIABLE || class == C_SIGNAL || class == C_FILE;

   if (is_pure_func && maybe_impure) {
      scope_t *owner = scope_containing(tab, decl);
      if (owner != NULL && scope_find_enclosing(owner, S_SUBPROGRAM) != sub) {
         error_at(tree_loc(ref), "invalid reference to %s inside pure "
                  "function %s", istr(tree_ident(decl)), istr(tree_ident(sub)));
      }
   }
}

////////////////////////////////////////////////////////////////////////////////
// Type solver

static void solve_subprogram_prefix(overload_t *o)
{
   if (o->name == NULL)
      o->name = tree_ident(o->tree);

   if (o->prefix == NULL) {
      ident_t full_name = o->name;
      for (;;) {
         o->name = ident_walk_selected(&full_name);
         if (full_name == NULL)
            break;

         if (o->prefix == NULL)
            o->prefix = scope_find(o->nametab->top_scope, o->name,
                                   NULL, NULL, 0);
         else
            o->prefix = search_decls(o->prefix, o->name, 0);

         if (o->prefix == NULL) {
            assert(error_count() > 0);  // Parser should have reported
            o->error = true;
            break;
         }
      }
   }
}

static void solve_one_param(nametab_t *tab, tree_t p, overload_t *o)
{
   param_kind_t subkind = tree_subkind(p);
   switch (subkind) {
   case P_POS:
      overload_positional_argument(o, tree_pos(p));
      break;
   case P_NAMED:
      {
         tree_t name = tree_name(p);
         overload_push_names(o);
         solve_types(tab, name, NULL);
         assert(tree_has_type(name));
         overload_named_argument(o, name);
      }
      break;
   }

   overload_next_argument(o, p);
}

static void solve_subprogram_params(nametab_t *tab, tree_t call, overload_t *o)
{
   const int nparams = tree_params(call);
   uint64_t pmask = 0;

   if (nparams > 64)
      fatal_at(tree_loc(call), "more than 64 parameters are not supported");

   // Make an initial pass over the parameters and solve the "easy" ones

   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(call, i);
      tree_t value = tree_value(p);
      tree_kind_t kind = tree_kind(value);

      const bool solve_now =
         tree_has_type(value)
         || kind == T_QUALIFIED
         || kind == T_ARRAY_REF
         || kind == T_ARRAY_SLICE
         || kind == T_TYPE_CONV
         || kind == T_ATTR_REF
         || kind == T_RECORD_REF
         || kind == T_ALL
         || (kind == T_REF && tree_has_ref(value));

      if (solve_now) {
         solve_one_param(tab, p, o);
         pmask |= (1 << i);
      }
   }

   // If there are still multiple candidates try to prune based on the
   // set of possible types for each remaining parameter

   for (int i = 0; i < nparams && o->candidates.count > 1; i++) {
      if (pmask & (1 << i))
         continue;

      tree_t p = tree_param(call, i);
      tree_t value = tree_value(p);
      tree_kind_t kind = tree_kind(value);

      if (kind == T_REF || kind == T_FCALL) {
         SCOPED_A(type_t) possible = AINIT;
         ident_t name = tree_ident(value);
         tree_t decl;
         int n = 0;
         while ((decl = scope_find(tab->top_scope, name, NULL, NULL, n++))) {
            if (decl == (void*)-1 || !class_has_type(class_of(decl)))
               break;

            type_t type1 = tree_type(decl);
            if (type_kind(type1) == T_FUNC)
               type1 = type_result(type1);

            type_t type2 = NULL;
            if (kind == T_FCALL && type_is_array(type1)) {
               // Grammar is ambiguous between indexed name and function call
               type2 = type_elem(type1);
            }

            bool have1 = false, have2 = false;
            for (unsigned j = 0; j < possible.count; j++) {
               have1 |= possible.items[j] == type1;
               have2 |= possible.items[j] == type2;
            }

            if (!have1) APUSH(possible, type1);
            if (type2 && !have2) APUSH(possible, type2);

            if (!can_overload(decl))
               break;
         }

         if (possible.count > 0)
            overload_restrict_argument(o, p, possible.items, possible.count);
      }
   }

   // Make two additional passes: one to do function calls and a final
   // catch-all

   for (int pass = 0; pass < 2; pass++) {
      for (int i = 0; i < nparams; i++) {
         if (pmask & (1 << i))
            continue;

         tree_t p = tree_param(call, i);
         tree_t value = tree_value(p);
         tree_kind_t kind = tree_kind(value);

         if (o->error && o->initial == 0) {
            // Avoid cascading errors from an undefined subprogram
            tree_set_type(value, type_new(T_NONE));
            pmask |= 1 << i;
            continue;
         }

         if (kind == T_FCALL || pass == 1) {
            solve_one_param(tab, p, o);
            pmask |= (1 << i);
         }
      }
   }
}

static bool can_call_no_args(nametab_t *tab, tree_t decl)
{
   if (tree_kind(decl) == T_ALIAS)
      return type_params(tree_type(decl)) == 0;
   else if (tree_ports(decl) == 0 || tree_has_value(tree_port(decl, 0)))
      return true;

   // The declaration may be overloaded and one of the overloads permits
   // calling with no arguments

   iter_state_t iter;
   begin_iter(tab, tree_ident(decl), &iter);

   while ((decl = iter_name(&iter)) && decl != (tree_t)-1) {
      if (!is_subprogram(decl))
         continue;
      else if (tree_ports(decl) == 0 || tree_has_value(tree_port(decl, 0)))
         return true;
   }

   return false;
}

static type_t solve_fcall(nametab_t *tab, tree_t fcall)
{
   if (tree_has_type(fcall))
      return tree_type(fcall);

   const tree_kind_t kind = tree_kind(fcall);

   overload_t o = {
      .tree     = fcall,
      .state    = O_IDLE,
      .nametab  = tab,
      .trace    = false,
      .prefix   = kind == T_PROT_FCALL ? tree_name(fcall) : NULL
   };
   solve_subprogram_prefix(&o);
   begin_overload_resolution(&o);

   solve_subprogram_params(tab, fcall, &o);

   type_t type;
   tree_t decl = finish_overload_resolution(&o);
   if (decl == NULL)
      type = type_new(T_NONE);
   else {
      type_t ftype = tree_type(decl);
      type_kind_t typek = type_kind(ftype);
      if (typek == T_PROC) {
         error_at(tree_loc(fcall), "procedure %s not allowed in an expression",
                  istr(tree_ident(fcall)));
         type = type_new(T_NONE);
      }
      else {
         assert(typek == T_FUNC);
         type = type_result(ftype);
         tree_set_ref(fcall, decl);

         // The expression A(X) can be parsed as a call to subprogram A
         // with no arguments, returning an array that is indexed by X,
         // or a call to subprogram A with argument X. We prefer the
         // later and then fix-up here based on the context.
         type_t context;
         if (type_is_array(type)
             && type_set_uniq(tab, &context)
             && !type_eq(context, type)
             && type_eq(context, type_elem(type))
             && can_call_no_args(tab, decl)) {

            tree_t new = tree_new(T_FCALL);
            tree_set_ref(new, decl);
            tree_set_ident(new, tree_ident(fcall));
            tree_set_loc(new, tree_loc(fcall));
            tree_set_type(new, type);

            tree_change_kind(fcall, T_ARRAY_REF);
            tree_set_value(fcall, new);

            type = type_elem(type);
         }
         else if ((tree_flags(decl) & TREE_F_PROTECTED) && kind != T_PROT_FCALL)
            tree_change_kind(fcall, T_PROT_FCALL);
      }
   }

   tree_set_type(fcall, type);
   return type;
}

static type_t solve_pcall(nametab_t *tab, tree_t pcall)
{
   const tree_kind_t kind = tree_kind(pcall);

   overload_t o = {
      .name     = tree_ident(pcall),
      .tree     = pcall,
      .state    = O_IDLE,
      .nametab  = tab,
      .trace    = false,
      .prefix   = kind == T_PROT_PCALL ? tree_name(pcall) : NULL
   };
   solve_subprogram_prefix(&o);
   begin_overload_resolution(&o);

   solve_subprogram_params(tab, pcall, &o);

   tree_t decl = finish_overload_resolution(&o);
   if (decl != NULL) {
      tree_set_ref(pcall, decl);
      if ((tree_flags(decl) & TREE_F_PROTECTED) && kind == T_PCALL)
         tree_change_kind(pcall, T_PROT_PCALL);
   }

   return NULL;  // Procedure call has no type
}

static bool is_character_array(type_t t)
{
   // According LRM 93 section 3.1.1 an enumeration type is a character
   // type if at least one of its enumeration literals is a character
   // literal

   if (!type_is_array(t))
      return false;

   if (dimension_of(t) != 1)
      return false;

   type_t elem = type_base_recur(type_elem(t));

   if (!type_is_enum(elem))
      return false;

   const int nlits = type_enum_literals(elem);
   for (int i = 0; i < nlits; i++) {
      tree_t lit = type_enum_literal(elem, i);
      if (ident_char(tree_ident(lit), 0) == '\'')
         return true;
   }

   return false;
}

static type_t solve_literal(nametab_t *tab, tree_t lit)
{
   if (tree_has_type(lit))
      return tree_type(lit);

   switch (tree_subkind(lit)) {
   case L_STRING:
      {
         // The type must be determinable soley from the context
         // excluding the literal itself but using the fact that the
         // type must be a one dimensional array of a character type

         type_t type = NULL;
         if (!type_set_restrict(tab, is_character_array)) {
            error_at(tree_loc(lit), "type of string literal cannot be "
                     "determined from context");
            type = type_new(T_NONE);
         }
         else if (!type_set_uniq(tab, &type)) {
            LOCAL_TEXT_BUF ts = type_set_fmt(tab);
            error_at(tree_loc(lit), "type of string literal is ambiguous%s",
                     tb_get(ts));
            type = type_new(T_NONE);
         }

         tree_set_type(lit, type);

         type_t elem = type_elem(type);
         const int nchars = tree_chars(lit);
         for (int i = 0; i < nchars; i++)
            solve_types(tab, tree_char(lit, i), elem);

         return type;
      }

   case L_NULL:
      {
         type_t type;
         if (!type_set_uniq(tab, &type)) {
            error_at(tree_loc(lit), "invalid use of null expression");
            type = type_new(T_NONE);
         }

         tree_set_type(lit, type);
         return type;
      }

   case L_PHYSICAL:
      {
         ident_t id = tree_ident(lit);
         type_t type;
         tree_t decl = resolve_name(tab, tree_loc(lit), id);
         if (decl == NULL)
            type = type_new(T_NONE);
         else if (tree_kind(decl) != T_UNIT_DECL) {
            error_at(tree_loc(lit), "%s is not a physical unit", istr(id));
            type = type_new(T_NONE);
         }
         else {
            tree_set_ref(lit, decl);
            type = tree_type(decl);
         }

         tree_set_type(lit, type);
         return type;
      }

   default:
      fatal_at(tree_loc(lit), "cannot solve literal");
   }
}

static type_t solve_ref(nametab_t *tab, tree_t ref)
{
   if (tree_has_type(ref))
      return tree_type(ref);

   tree_t decl = resolve_ref(tab, ref);

   type_t type;
   if (decl == NULL)
      tree_set_type(ref, (type = type_new(T_NONE)));
   else if (!class_has_type(class_of(decl))) {
      error_at(tree_loc(ref), "invalid use of %s %s",
               class_str(class_of(decl)), istr(tree_ident(ref)));
      tree_set_ref(ref, NULL);
      tree_set_type(ref, (type = type_new(T_NONE)));
   }
   else if (is_subprogram(decl)) {
      type_t constraint;
      const bool want_ref =
         type_set_uniq(tab, &constraint) && type_is_subprogram(constraint);

      if (!can_call_no_args(tab, decl) || want_ref) {
         // We want a reference to the subprogram not a call to it with
         // no arguments
         tree_set_ref(ref, decl);
         tree_set_type(ref, (type = tree_type(decl)));
      }
      else {
         tree_change_kind(ref, T_FCALL);
         type = solve_fcall(tab, ref);
      }
   }
   else {
      check_pure_ref(tab, ref, decl);
      tree_set_ref(ref, decl);
      if (tree_kind(decl) == T_ALIAS && !tree_has_type(decl))
         tree_set_type(ref, (type = tree_type(tree_value(decl))));
      else
         tree_set_type(ref, (type = tree_type(decl)));
   }

   return type;
}

static type_t solve_record_ref(nametab_t *tab, tree_t rref)
{
   if (tree_has_type(rref))
      return tree_type(rref);

   type_t value_type = solve_types(tab, tree_value(rref), NULL);

   if (type_is_none(value_type)) {
      tree_set_type(rref, value_type);
      return value_type;
   }

   tree_t field = find_record_field(rref);

   type_t type;
   if (field == NULL) {
      error_at(tree_loc(rref), "record type %s has no field named %s",
               type_pp(value_type), istr(tree_ident(rref)));
      type = type_new(T_NONE);
   }
   else
      type = tree_type(field);

   tree_set_ref(rref, field);
   tree_set_type(rref, type);
   return type;
}

static type_t solve_array_ref(nametab_t *tab, tree_t ref)
{
   if (tree_has_type(ref))
      return tree_type(ref);

   type_t base_type = solve_types(tab, tree_value(ref), NULL);

   type_t elem_type;
   if (type_is_array(base_type))
      elem_type = type_elem(base_type);
   else
      elem_type = type_new(T_NONE);

   tree_set_type(ref, elem_type);
   return elem_type;
}

static type_t solve_array_slice(nametab_t *tab, tree_t slice)
{
   if (tree_has_type(slice))
      return tree_type(slice);

   type_t base_type = _solve_types(tab, tree_value(slice));

   tree_t r = tree_range(slice, 0);

   tree_t constraint = tree_new(T_CONSTRAINT);
   tree_set_subkind(constraint, C_INDEX);
   tree_add_range(constraint, r);

   type_t slice_type = type_new(T_SUBTYPE);
   type_set_base(slice_type, base_type);
   type_set_constraint(slice_type, constraint);

   tree_set_type(slice, slice_type);
   return slice_type;
}

static type_t solve_attr_ref(nametab_t *tab, tree_t aref)
{
   if (tree_has_type(aref))
      return tree_type(aref);

   tree_t prefix = tree_name(aref);
   type_t prefix_type = NULL;
   if (tree_kind(prefix) == T_REF && !tree_has_type(prefix)) {
      tree_t decl;
      if (tree_has_ref(prefix))
         decl = tree_ref(prefix);
      else if ((decl = resolve_ref(tab, prefix)))
         tree_set_ref(prefix, decl);

      if (decl != NULL) {
         if (class_has_type(class_of(decl)))
            tree_set_type(prefix, (prefix_type = tree_type(decl)));
      }
   }
   else if (type_is_none((prefix_type = solve_types(tab, prefix, NULL)))) {
      tree_set_type(aref, prefix_type);
      return prefix_type;
   }

   const int nparams = tree_params(aref);
   assert(nparams <= 1);
   if (nparams == 1) {
      type_set_push(tab);

      switch (tree_subkind(aref)) {
      case ATTR_IMAGE:
      case ATTR_LEFTOF:
      case ATTR_RIGHTOF:
      case ATTR_POS:
         type_set_add(tab, prefix_type);
         break;
      case ATTR_VALUE:
         type_set_add(tab, std_type(NULL, STD_STRING));
         break;
      }

      _solve_types(tab, tree_value(tree_param(aref, 0)));
      type_set_pop(tab);
   }

   type_t type = NULL;
   switch (tree_subkind(aref)) {
   case ATTR_LENGTH:
      type = std_type(NULL, STD_INTEGER);
      break;

   case ATTR_LEFT:
   case ATTR_RIGHT:
   case ATTR_LOW:
   case ATTR_HIGH:
   case ATTR_RANGE:
   case ATTR_REVERSE_RANGE: {
      type = prefix_type ?: type_new(T_NONE);

      if (type != NULL && type_is_array(type)) {
         int dim = 0;
         if (nparams > 0) {
            tree_t pdim = tree_value(tree_param(aref, 0));
            if (tree_kind(pdim) == T_LITERAL) // XXX: what if real or physical?
               dim = tree_ival(pdim) - 1;
         }

         type = index_type_of(type, dim);
      }
   } break;

   case ATTR_LAST_EVENT:
   case ATTR_LAST_ACTIVE:
      type = std_type(NULL, STD_TIME);
      break;

   case ATTR_ASCENDING:
   case ATTR_EVENT:
   case ATTR_ACTIVE:
   case ATTR_STABLE:
   case ATTR_QUIET:
   case ATTR_DRIVING:
      type = std_type(NULL, STD_BOOLEAN);
      break;

   case ATTR_LEFTOF:
   case ATTR_RIGHTOF:
   case ATTR_PRED:
   case ATTR_SUCC:
   case ATTR_VAL:
   case ATTR_DELAYED:
   case ATTR_LAST_VALUE:
   case ATTR_VALUE:
   case ATTR_DRIVING_VALUE:
      type = prefix_type;
      break;

   case ATTR_PATH_NAME:
   case ATTR_INSTANCE_NAME:
   case ATTR_SIMPLE_NAME:
   case ATTR_IMAGE:
      type = std_type(NULL, STD_STRING);
      break;

   case ATTR_TRANSACTION:
      type = std_type(NULL, STD_BIT);
      break;

   case ATTR_POS:
      type = std_type(NULL, STD_UNIVERSAL_INTEGER);
      break;

   case ATTR_BASE:
      if (prefix_type != NULL) {
         if (type_kind(prefix_type) == T_SUBTYPE)
            type = type_base(prefix_type);
         else
            type = prefix_type;
      }
      break;

   case ATTR_USER:
      {
         if (tree_kind(prefix) == T_REF) {
            tree_t decl = tree_ref(prefix);
            ident_t id = tree_ident(decl);
            ident_t attr = tree_ident(aref);
            class_t class = class_of(decl);
            tree_t a = NULL;

            // Hack to strip off any WORK. prefix
            id = ident_rfrom(id, '.') ?: id;

            if (is_container(decl)) {
               for (int n = 0; (a = search_decls(decl, attr, n)); n++) {
                  if (tree_kind(a) == T_ATTR_SPEC
                      && tree_class(a) == class
                      && tree_ident2(a) == id)
                     break;
               }
            }
            else {
               // TODO: this isn't right, should only search same region as decl
               iter_state_t iter;
               begin_iter(tab, attr, &iter);

               while ((a = iter_name(&iter)) && a != (tree_t)-1) {
                  if (tree_kind(a) == T_ATTR_SPEC
                      && tree_class(a) == class
                      && tree_ident2(a) == id)
                     break;
               }
            }

            if (a != NULL) {
               tree_set_value(aref, tree_value(a));
               type = tree_type(tree_ref(a));
            }
            else {
               error_at(tree_loc(prefix), "object %s has no attribute %s",
                        istr(tree_ident(prefix)), istr(tree_ident(aref)));
               type = type_new(T_NONE);
            }
         }
         else {
            error_at(tree_loc(prefix), "prefix of user defined attribute "
                     "reference cannot denote a sub-element or slice of an "
                     "object");
            type = type_new(T_NONE);
         }
      }
      break;

   default:
      fatal_trace("unhandled attribute %s", istr(tree_ident(aref)));
   }

   tree_set_type(aref, type);
   return type;
}

static type_t solve_aggregate(nametab_t *tab, tree_t agg)
{
   if (tree_has_type(agg))
      return tree_type(agg);

   // The type of an aggregate must be determinable solely from the
   // context in which the aggregate appears

   type_t type;
   if (type_set_uniq(tab, &type)) {
      if (!type_is_composite(type) && !type_is_none(type)) {
         error_at(tree_loc(agg), "aggregate has non-composite type %s",
                  type_pp(type));
         type = type_new(T_NONE);
      }
   }
   else if (!type_set_restrict(tab, type_is_composite)) {
      error_at(tree_loc(agg), "type of aggregate cannot be determined "
               "from context");
      type = type_new(T_NONE);
   }
   else if (!type_set_uniq(tab, &type)) {
      LOCAL_TEXT_BUF ts = type_set_fmt(tab);
      error_at(tree_loc(agg), "type of aggregate is ambiguous%s", tb_get(ts));
      type = type_new(T_NONE);
   }

   tree_set_type(agg, type);

   if (type_is_record(type)) {
      const int nfields = type_fields(type);
      const int nassocs = tree_assocs(agg);

      // Mask used for finding the types of an "others" association.
      // This won't work with more than 64 fields.
      uint64_t fmask = 0;

      for (int i = 0; i < nassocs; i++) {
         type_set_push(tab);
         tree_t a = tree_assoc(agg, i);

         switch ((assoc_kind_t)tree_subkind(a)) {
         case A_POS:
            {
               int pos = tree_pos(a);
               if (pos < nfields)
                  type_set_add(tab, tree_type(type_field(type, pos)));
               if (pos < 64) fmask |= (1 << pos);
            }
            break;

         case A_NAMED:
            {
               push_scope(tab);
               scope_set_formal_kind(tab, agg, F_RECORD);
               tree_t name = tree_name(a);
               solve_types(tab, name, NULL);
               pop_scope(tab);
               if (tree_has_ref(name)) {
                  tree_t field = tree_ref(name);
                  type_set_add(tab, tree_type(field));
                  if (tree_kind(field) == T_FIELD_DECL) {
                     const int pos = tree_pos(field);
                     if (pos < 64) fmask |= (1 << pos);
                  }
               }
            }
            break;

         case A_OTHERS:
            // Add the types of all the fields that haven't already be
            // specified to the type set
            for (int i = 0; i < MIN(nfields, 64); i++) {
               if (!(fmask & (1 << i)))
                  type_set_add(tab, tree_type(type_field(type, i)));
            }
            break;

         case A_RANGE:
            // This is illegal and will generate an error during
            // semantic checking
            push_scope(tab);
            scope_set_formal_kind(tab, agg, F_RECORD);
            solve_types(tab, tree_range(a, 0), NULL);
            pop_scope(tab);
            break;
         }

         _solve_types(tab, a);
         type_set_pop(tab);
      }
   }
   else {
      // All elements must be of the composite base type if this is a
      // one-dimensional array otherwise construct an array type with
      // n-1 dimensions.

      type_set_push(tab);

      const int ndims = dimension_of(type);
      if (ndims == 1)
         type_set_add(tab, type_elem(type));
      else
         type_set_add(tab, array_aggregate_type(type, 1));

      type_t index_type = index_type_of(type, 0);

      const int nassocs = tree_assocs(agg);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(agg, i);

         switch ((assoc_kind_t)tree_subkind(a)) {
         case A_POS:
         case A_OTHERS:
            break;
         case A_NAMED:
            solve_types(tab, tree_name(a), index_type);
            break;
         case A_RANGE:
            solve_types(tab, tree_range(a, 0), index_type);
            break;
         }

         _solve_types(tab, tree_value(a));
      }

      type_set_pop(tab);
   }

   return type;
}

static type_t solve_assoc(nametab_t *tab, tree_t assoc)
{
   switch (tree_subkind(assoc)) {
   case A_NAMED:
      _solve_types(tab, tree_name(assoc));
      break;
   }

   return _solve_types(tab, tree_value(assoc));
}

static type_t solve_param(nametab_t *tab, tree_t param)
{
   switch (tree_subkind(param)) {
   case P_NAMED:
      _solve_types(tab, tree_name(param));
      break;
   }

   return _solve_types(tab, tree_value(param));
}

static type_t solve_qualified(nametab_t *tab, tree_t qual)
{
   assert(tree_has_type(qual));

   solve_types(tab, tree_value(qual), tree_type(qual));

   return tree_type(qual);
}

static type_t solve_type_conv(nametab_t *tab, tree_t expr)
{
   solve_types(tab, tree_value(expr), NULL);
   return tree_type(expr);
}

static type_t solve_new(nametab_t *tab, tree_t new)
{
   if (tree_has_type(new))
      return tree_type(new);

   type_t type = NULL;
   if (!type_set_restrict(tab, type_is_access)) {
      error_at(tree_loc(new), "cannot determine type of allocator expression "
               "from context");
      type = type_new(T_NONE);
   }
   else if (!type_set_uniq(tab, &type)) {
      LOCAL_TEXT_BUF ts = type_set_fmt(tab);
      error_at(tree_loc(new), "type of allocator expression is ambiguous%s",
               tb_get(ts));
      type = type_new(T_NONE);
   }

   tree_set_type(new, type);
   return type;
}

static type_t solve_all(nametab_t *tab, tree_t all)
{
   if (tree_has_type(all))
      return tree_type(all);

   tree_t prefix = tree_value(all);

   type_t type, access = solve_types(tab, prefix, NULL);
   if (type_is_access(access)) {
      type = type_access(access);
      if (type_is_incomplete(type)
          && (type = resolve_type(tab, type)) == NULL) {
         error_at(tree_loc(prefix), "object with incomplete type %s "
                  "cannot be dereferenced", type_pp(type));
         type = type_new(T_NONE);
      }
   }
   else
      type = type_new(T_NONE);

   tree_set_type(all, type);
   return type;
}

static type_t solve_open(nametab_t *tab, tree_t open)
{
   if (tree_has_type(open))
      return tree_type(open);

   type_t type;
   if (!type_set_uniq(tab, &type)) {
      error_at(tree_loc(open), "cannot determine type of OPEN expression");
      type = type_new(T_NONE);
   }

   tree_set_type(open, type);
   return type;
}

static type_t solve_range(nametab_t *tab, tree_t r)
{
   type_t type = NULL;
   switch (tree_subkind(r)) {
   case RANGE_ERROR:
      type = type_new(T_NONE);
      break;
   case RANGE_EXPR:
      type = _solve_types(tab, tree_value(r));
      break;
   case RANGE_TO:
   case RANGE_DOWNTO:
      {
         tree_t left = tree_left(r);
         tree_t right = tree_right(r);

         // Potentially swap the argument order for checking if the
         // right type can be determined unambiguously
         tree_kind_t rkind = tree_kind(right);
         const bool swap =
            (tree_has_type(right) && !type_is_universal(tree_type(right)))
            || rkind == T_QUALIFIED
            || rkind == T_ARRAY_REF
            || rkind == T_ARRAY_SLICE
            || rkind == T_TYPE_CONV
            || (rkind == T_REF && query_name(tab, tree_ident(right)));

         if (swap) { tree_t tmp = left; left = right; right = tmp; }

         type = _solve_types(tab, left);

         if (tab->top_type_set->members.count == 0)
            type_set_add(tab, type);

         _solve_types(tab, right);
         break;
      }
   default:
      assert(false);
   }

   tree_set_type(r, type);
   return type;
}

static type_t _solve_types(nametab_t *tab, tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_FCALL:
   case T_PROT_FCALL:
      return solve_fcall(tab, expr);
   case T_PCALL:
   case T_PROT_PCALL:
      return solve_pcall(tab, expr);
   case T_LITERAL:
      return solve_literal(tab, expr);
   case T_REF:
      return solve_ref(tab, expr);
   case T_RECORD_REF:
      return solve_record_ref(tab, expr);
   case T_AGGREGATE:
      return solve_aggregate(tab, expr);
   case T_ARRAY_REF:
      return solve_array_ref(tab, expr);
   case T_ARRAY_SLICE:
      return solve_array_slice(tab, expr);
   case T_ASSOC:
      return solve_assoc(tab, expr);
   case T_PARAM:
      return solve_param(tab, expr);
   case T_ATTR_REF:
      return solve_attr_ref(tab, expr);
   case T_QUALIFIED:
      return solve_qualified(tab, expr);
   case T_TYPE_CONV:
      return solve_type_conv(tab, expr);
   case T_OPEN:
      return solve_open(tab, expr);
   case T_NEW:
      return solve_new(tab, expr);
   case T_ALL:
      return solve_all(tab, expr);
   case T_RANGE:
      return solve_range(tab, expr);
   case T_TYPE_REF:
      return tree_type(expr);
   default:
      fatal_trace("cannot solve types for %s", tree_kind_str(tree_kind(expr)));
   }

   return type_new(T_NONE);
}

type_t solve_types(nametab_t *tab, tree_t expr, type_t constraint)
{
   type_set_push(tab);
   type_set_add(tab, constraint);
   type_t type = _solve_types(tab, expr);
   type_set_pop(tab);
   return type;
}

type_t solve_condition(nametab_t *tab, tree_t expr, type_t constraint)
{
   type_set_push(tab);
   type_set_add(tab, constraint);

   if (standard() >= STD_08)
      tab->top_type_set->flags |= TS_CONDITION_CONVERSION;

   type_t type = _solve_types(tab, expr);
   type_set_pop(tab);
   return type;
}
