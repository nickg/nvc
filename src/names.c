//
//  Copyright (C) 2020-2023  Nick Gasson
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
#include "debug.h"
#include "diag.h"
#include "hash.h"
#include "lib.h"
#include "mask.h"
#include "names.h"
#include "option.h"
#include "phase.h"
#include "thread.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

typedef struct scope scope_t;
typedef struct _type_set type_set_t;
typedef struct _spec spec_t;
typedef struct _sym_chunk sym_chunk_t;
typedef struct _lazy_sym lazy_sym_t;

typedef A(tree_t) tree_list_t;
typedef A(type_t) type_list_t;

struct _spec {
   spec_t      *next;
   spec_kind_t  kind;
   ident_t      ident;
   tree_t       tree;
   unsigned     matches;
};

typedef enum {
   DIRECT, POTENTIAL, HIDDEN, OVERLOAD, ATTRIBUTE
} visibility_t;

typedef struct {
   tree_t        tree;
   scope_t      *origin;
   visibility_t  visibility : 16;
   tree_kind_t   kind : 16;
   name_mask_t   mask;
} decl_t;

#define INLINE_DECLS 4

typedef struct {
   ident_t      name;
   scope_t     *owner;
   name_mask_t  mask;
   unsigned     ndecls;
   unsigned     overflowsz;
   decl_t       decls[INLINE_DECLS];
   decl_t      *overflow;
} symbol_t;

#define SYMBOLS_PER_CHUNK 32

typedef struct _sym_chunk {
   sym_chunk_t *chain;
   unsigned     count;
   symbol_t     symbols[SYMBOLS_PER_CHUNK];
} sym_chunk_t;

typedef enum {
   O_IDLE,
   O_POS,
   O_NAMED,
} overload_state_t;

typedef struct {
   ident_t           name;
   tree_t            tree;
   tree_list_t       candidates;
   tree_list_t       params;
   nametab_t        *nametab;
   const symbol_t   *symbol;
   overload_state_t  state;
   bool              error;
   bool              trace;
   bool              trial;
   bool              could_be_index;
   bool              implicit_conversion;
   bool              cancelled;
   type_t            signature;
   tree_t            prefix;
   unsigned          initial;
   unsigned          nactuals;
} overload_t;

typedef symbol_t *(*lazy_fn_t)(scope_t *, ident_t, void *);
typedef void (*formal_fn_t)(diag_t *, ident_t, void *);
typedef void (*make_visible_t)(scope_t *, ident_t, tree_t);

typedef struct _lazy_sym {
   lazy_sym_t *next;
   lazy_fn_t   fn;
   void       *ctx;
} lazy_sym_t;

typedef struct {
   int proc;
   int loop;
} label_cnts_t;

typedef struct {
   defer_check_fn_t fn;
   tree_t           tree;
} defer_check_t;

typedef A(defer_check_t) defer_checks_t;

struct scope {
   scope_t        *parent;
   sym_chunk_t     symbols;
   sym_chunk_t    *sym_tail;
   hash_t         *lookup;
   hash_t         *gmap;
   spec_t         *specs;
   overload_t     *overload;
   formal_kind_t   formal_kind;
   formal_fn_t     formal_fn;
   void           *formal_arg;
   ident_t         prefix;
   tree_t          container;
   bool            suppress;
   lazy_sym_t     *lazy;
   tree_list_t     imported;
   label_cnts_t    lbl_cnts;
   scope_t        *chain;
   defer_checks_t  deferred;
};

typedef struct _nametab {
   scope_t    *top_scope;
   type_set_t *top_type_set;
   hash_t     *globalmap;
   scope_t    *globals;
   tree_t      std;
   tree_t      psl;
} nametab_t;

typedef struct {
   type_t type;
   tree_t src;
} tracked_type_t;

typedef A(tracked_type_t) tracked_type_list_t;

typedef bool (*type_pred_t)(type_t);

typedef struct _type_set {
   tracked_type_list_t  members;
   type_set_t          *down;
   type_pred_t          pred;
   unsigned             watermark;
   bool                 known_subtype;
} type_set_t;

static type_t _solve_types(nametab_t *tab, tree_t expr);
static type_t try_solve_type(nametab_t *tab, tree_t expr);
static bool is_forward_decl(tree_t decl, tree_t existing);
static bool denotes_same_object(tree_t a, tree_t b);
static void make_visible_slow(scope_t *s, ident_t name, tree_t decl);
static const symbol_t *iterate_symbol_for(nametab_t *tab, ident_t name);
static void free_scope(scope_t *s);

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

static void type_set_describe(nametab_t *tab, diag_t *d, const loc_t *loc,
                              type_pred_t pred, const char *hint)
{
   const type_set_t *ts = tab->top_type_set;
   if (ts == NULL)
      return;

   const int count = ts->members.count ?: ts->watermark;
   if (count == 0)
      return;

   LOCAL_TEXT_BUF tb = tb_new();

   int poss = 0;
   for (unsigned n = 0; n < count; n++) {
      tracked_type_t tt = ts->members.items[n];

      if (type_is_none(tt.type))
         continue;
      else if (pred == NULL || (*pred)(tt.type)) {
         if (n > 0 && n + 1 == count)
            tb_cat(tb, " or ");
         else if (n > 0)
            tb_cat(tb, ", ");

         tb_cat(tb, type_pp(tt.type));
         poss++;
      }
      else if (hint != NULL && *hint != '\0')
         diag_hint(d, NULL, "context contains type %s which is not %s",
                   type_pp(tt.type), hint);
      else
         diag_hint(d, NULL, "context contains type %s", type_pp(tt.type));

      if (tt.src != NULL && is_subprogram(tt.src))
         diag_hint(d, tree_loc(tt.src), "context contains overload %s",
                   type_pp(tree_type(tt.src)));
   }

   if (poss > 0)
      diag_hint(d, loc, "%s %s", poss > 1 ? "could be" : "expecting",
                tb_get(tb));
}

static void type_set_add(nametab_t *tab, type_t t, tree_t src)
{
   assert(tab->top_type_set != NULL);

   if (t == NULL)
      return;

   if (type_kind(t) == T_INCOMPLETE)
      t = resolve_type(tab, t);

   if (tab->top_scope->gmap != NULL)
      t = hash_get(tab->top_scope->gmap, t) ?: t;

   for (unsigned i = 0; i < tab->top_type_set->members.count; i++) {
      if (type_eq(tab->top_type_set->members.items[i].type, t))
         return;
   }

   APUSH(tab->top_type_set->members, ((tracked_type_t){t, src}));
}

static void type_set_restrict(nametab_t *tab, type_pred_t pred)
{
   if (tab->top_type_set == NULL)
      return;

   tab->top_type_set->watermark = tab->top_type_set->members.count;

   int j = 0;
   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      tracked_type_t tt = tab->top_type_set->members.items[i];
      if ((*pred)(tt.type))
         tab->top_type_set->members.items[j++] = tt;
   }
   ATRIM(tab->top_type_set->members, j);
}

static bool type_set_uniq(nametab_t *tab, type_t *pt)
{
   assert(tab->top_type_set != NULL);

   if (tab->top_type_set->members.count == 1) {
      *pt = tab->top_type_set->members.items[0].type;
      return true;
   }
   else {
      *pt = NULL;
      return false;
   }
}

static bool type_set_contains(nametab_t *tab, type_t type)
{
   if (tab->top_type_set == NULL || tab->top_type_set->members.count == 0)
      return true;

   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      type_t member = tab->top_type_set->members.items[i].type;
      if (type_eq(type, member))
         return true;
   }

   return false;
}

static bool type_set_any(nametab_t *tab, type_pred_t pred)
{
   if (tab->top_type_set == NULL)
      return false;

   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      if ((*pred)(tab->top_type_set->members.items[i].type))
         return true;
   }

   return false;
}

////////////////////////////////////////////////////////////////////////////////
// Scopes

static bool can_overload(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ALIAS:
      {
         tree_t alias = tree_value(t);
         if (tree_kind(alias) != T_REF)
            return false;
         else if (!tree_has_ref(alias))
            return true;   // Suppress cascading errors
         else
            return can_overload(tree_ref(alias));
      }
   case T_GENERIC_DECL:
      {
         const class_t class = tree_class(t);
         return class == C_FUNCTION || class == C_PROCEDURE;
      }
   case T_ENUM_LIT:
   case T_UNIT_DECL:
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_PROC_DECL:
   case T_PROC_BODY:
   case T_PROC_INST:
   case T_FUNC_INST:
      return true;
   default:
      return false;
   }
}

nametab_t *nametab_new(void)
{
   nametab_t *tab = xcalloc(sizeof(nametab_t));
   tab->globalmap = hash_new(128);
   return tab;
}

void nametab_finish(nametab_t *tab)
{
   assert(tab->top_scope == NULL);
   assert(tab->top_type_set == NULL);

   for (scope_t *s = tab->globals, *tmp; s; s = tmp) {
      tmp = s->chain;
      free_scope(s);
   }

   hash_free(tab->globalmap);
   free(tab);
}

void push_scope(nametab_t *tab)
{
   scope_t *s = xcalloc(sizeof(scope_t));
   s->lookup   = hash_new(128);
   s->parent   = tab->top_scope;
   s->prefix   = tab->top_scope ? tab->top_scope->prefix : NULL;
   s->suppress = tab->top_scope ? tab->top_scope->suppress : false;
   s->sym_tail = &(s->symbols);

   tab->top_scope = s;
}

static void free_overflow(sym_chunk_t *chunk)
{
   for (int i = 0; i < chunk->count; i++) {
      if (chunk->symbols[i].overflow)
         free(chunk->symbols[i].overflow);
   }
}

static void free_scope(scope_t *s)
{
   hash_free(s->lookup);

   free_overflow(&(s->symbols));

   for (sym_chunk_t *c = s->symbols.chain, *tmp; c; c = tmp) {
      free_overflow(c);
      tmp = c->chain;
      free(c);
   }

   for (lazy_sym_t *it = s->lazy, *tmp; it; it = tmp) {
      tmp = it->next;
      free(it);
   }

   hash_free(s->gmap);
   ACLEAR(s->imported);

   free(s);
}

void pop_scope(nametab_t *tab)
{
   assert(tab->top_scope != NULL);
   scope_t *tmp = tab->top_scope->parent;

   for (int i = 0; i < tab->top_scope->deferred.count; i++) {
      const defer_check_t *dc = &(tab->top_scope->deferred.items[i]);
      (*dc->fn)(dc->tree, tab->top_scope->container, tab);
   }
   ACLEAR(tab->top_scope->deferred);

   for (spec_t *it = tab->top_scope->specs, *next; it != NULL; it = next) {
      if (it->kind == SPEC_EXACT && it->matches == 0
          && !tab->top_scope->suppress)
         error_at(tree_loc(it->tree), "instance %s not found", istr(it->ident));

      next = it->next;
      free(it);
   }

   free_scope(tab->top_scope);
   tab->top_scope = tmp;
}

void suppress_errors(nametab_t *tab)
{
   tab->top_scope->suppress = true;
}

void defer_check(nametab_t *tab, defer_check_fn_t fn, tree_t t)
{
   defer_check_t dc = { fn, t };
   APUSH(tab->top_scope->deferred, dc);
}

void map_generic_type(nametab_t *tab, type_t generic, type_t actual)
{
   assert(type_kind(generic) == T_GENERIC);

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128);

   // Try to prevent large numbers of cascading errors when the actual
   // type is not of the correct class: sem_check_generic_actual will
   // report a single error later

   static bool (*const pred[])(type_t) = {
      [GTYPE_PRIVATE] = type_is_valid,
      [GTYPE_INTEGER] = type_is_integer,
      [GTYPE_DISCRETE] = type_is_discrete,
      [GTYPE_SCALAR] = type_is_scalar,
      [GTYPE_ARRAY] = type_is_array,
      [GTYPE_ACCESS] = type_is_access,
      [GTYPE_FILE] = type_is_file,
      [GTYPE_FLOATING] = type_is_real,
   };

   const gtype_class_t class = type_subkind(generic);
   if (pred[class] != NULL && !(*pred[class])(actual))
      suppress_errors(tab);

   hash_put(tab->top_scope->gmap, generic, actual);
}

void map_generic_subprogram(nametab_t *tab, tree_t decl, tree_t actual)
{
   assert(is_subprogram(actual));

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128);

   hash_put(tab->top_scope->gmap, decl, actual);
}

void map_generic_package(nametab_t *tab, tree_t generic, tree_t actual)
{
   assert(tree_kind(actual) == T_PACK_INST);

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128);

   const int ndecls = tree_decls(generic);
   for (int i = 0; i < ndecls; i++) {
      tree_t gd = tree_decl(generic, i);
      tree_t ad = tree_decl(actual, i);
      assert(tree_kind(gd) == tree_kind(ad));

      hash_put(tab->top_scope->gmap, gd, ad);

      if (is_type_decl(gd))
         hash_put(tab->top_scope->gmap, tree_type(gd), tree_type(ad));
   }

   const int ngenerics = tree_generics(generic);
   for (int i = 0; i < ngenerics; i++) {
      tree_t gg = tree_generic(generic, i);
      tree_t ag = tree_generic(actual, i);
      hash_put(tab->top_scope->gmap, gg, ag);

      if (tree_class(gg) == C_TYPE)
         hash_put(tab->top_scope->gmap, tree_type(gg), tree_type(ag));
   }
}

hash_t *get_generic_map(nametab_t *tab)
{
   return tab->top_scope->gmap;
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
      else if (what == S_CONCURRENT_BLOCK && is_concurrent_block(s->container))
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

bool is_enclosing(nametab_t *tab, tree_t container)
{
   assert(is_container(container));

   for (scope_t *s = tab->top_scope; s; s = s->parent) {
      if (s->container == container)
         return true;
   }

   return false;
}

formal_kind_t scope_formal_kind(nametab_t *tab)
{
   return tab->top_scope->formal_kind;
}

static const symbol_t *symbol_for(scope_t *s, ident_t name)
{
   do {
      symbol_t *sym = hash_get(s->lookup, name);
      if (sym != NULL)
         return sym;
   } while (s->formal_kind != F_RECORD && (s = s->parent));

   return NULL;
}

static const symbol_t *lazy_symbol_for(scope_t *s, ident_t name)
{
   do {
      symbol_t *sym = hash_get(s->lookup, name);
      if (sym != NULL)
         return sym;
      else {
         for (lazy_sym_t *it = s->lazy; it; it = it->next) {
            if ((sym = (*it->fn)(s, name, it->ctx)))
               return sym;
         }
      }
   } while (s->formal_kind != F_RECORD && (s = s->parent));

   return NULL;
}

static inline const decl_t *get_decl(const symbol_t *sym, unsigned nth)
{
   assert(nth < sym->ndecls);
   return (nth < INLINE_DECLS)
      ? &(sym->decls[nth]) : &(sym->overflow[nth - INLINE_DECLS]);
}

static inline decl_t *get_decl_mutable(symbol_t *sym, unsigned nth)
{
   assert(nth < sym->ndecls);
   return (nth < INLINE_DECLS)
      ? &(sym->decls[nth]) : &(sym->overflow[nth - INLINE_DECLS]);
}

static decl_t *add_decl(symbol_t *sym)
{
   if (sym->ndecls < INLINE_DECLS)
      return &(sym->decls[sym->ndecls++]);
   else if (sym->ndecls - INLINE_DECLS == sym->overflowsz) {
      sym->overflowsz = MAX(sym->overflowsz * 2, 32);
      sym->overflow = xrealloc_array(sym->overflow, sym->overflowsz,
                                     sizeof(decl_t));
   }
   else
      assert(sym->ndecls - INLINE_DECLS < sym->overflowsz);

   return &(sym->overflow[sym->ndecls++ - INLINE_DECLS]);
}

static symbol_t *local_symbol_for(scope_t *s, ident_t name)
{
   symbol_t *sym = hash_get(s->lookup, name);
   if (sym == NULL) {
      sym_chunk_t *chunk = s->sym_tail;
      if (chunk->count == SYMBOLS_PER_CHUNK) {
         chunk = s->sym_tail->chain = xcalloc(sizeof(sym_chunk_t));
         s->sym_tail = chunk;
      }

      sym = &(chunk->symbols[chunk->count++]);
      sym->name   = name;
      sym->owner  = s;
      sym->ndecls = 0;

      if (s->parent != NULL && s->formal_kind == F_NONE) {
         const symbol_t *exist = symbol_for(s->parent, name);
         if (exist != NULL) {
            for (int i = 0; i < exist->ndecls; i++)
               *add_decl(sym) = *get_decl(exist, i);
         }
      }

      hash_put(s->lookup, name, sym);
   }

   return sym;
}

static name_mask_t name_mask_for(tree_t t)
{
   switch (tree_kind(t)) {
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
   case T_PARAM_DECL:
   case T_CONST_DECL:
   case T_FIELD_DECL:
   case T_IMPLICIT_SIGNAL:
   case T_EXTERNAL_NAME:
      return N_OBJECT;
   case T_FUNC_BODY:
   case T_FUNC_DECL:
   case T_FUNC_INST:
      return N_FUNC;
   case T_PROC_BODY:
   case T_PROC_DECL:
   case T_PROC_INST:
      return N_PROC;
   case T_TYPE_DECL:
   case T_SUBTYPE_DECL:
   case T_PROT_DECL:
   case T_PROT_BODY:
      return N_TYPE;
   case T_GENERIC_DECL:
      {
         switch (class_of(t)) {
         case C_TYPE: return N_TYPE;
         case C_FUNCTION: return N_FUNC;
         case C_PROCEDURE: return N_PROC;
         default: return N_OBJECT;
         }
      }
   case T_ALIAS:
      {
         switch (class_of(tree_value(t))) {
         case C_TYPE:
         case C_SUBTYPE: return N_TYPE;
         case C_FUNCTION: return N_FUNC;
         case C_PROCEDURE: return N_PROC;
         case C_LABEL: return N_LABEL;
         case C_SIGNAL:
         case C_CONSTANT:
         case C_VARIABLE: return N_OBJECT;
         default: return 0;
         }
      }
   case T_PSL:
      return N_PSL;
   default:
      return 0;
   }
}

static void add_type_literals(scope_t *s, type_t type, make_visible_t fn)
{
   switch (type_kind(type)) {
   case T_ENUM:
      {
         const int nlits = type_enum_literals(type);
         for (int i = 0; i < nlits; i++) {
            tree_t literal = type_enum_literal(type, i);
            ident_t lit_name = tree_ident(literal);

            (*fn)(s, lit_name, literal);
         }
      }
      break;

   case T_PHYSICAL:
      {
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++) {
            tree_t unit = type_unit(type, i);
            ident_t unit_name = tree_ident(unit);

            (*fn)(s, unit_name, unit);
         }
      }
      break;

   default:
      break;
   }
}

static void warn_hidden_decl(scope_t *s, decl_t *outer, tree_t inner)
{
   // Warn when an inner declaration hides an outer declaration but try
   // to avoid false-positives and cases that may be intentional

   for (scope_t *it = s; it && it != outer->origin; it = it->parent) {
      if (s->container == NULL)
         return;   // Ignore ports in component, etc.
      else if (is_subprogram(it->container))
         return;   // Do not warn when subprogram parameters hide ports
      else if (tree_kind(it->container) == T_BLOCK)
         return;   // Block ports hiding entity ports seem common and benign
   }

   type_t outer_type = get_type_or_null(outer->tree);
   if (outer_type == NULL)
      return;

   type_t inner_type = get_type_or_null(inner);
   if (inner_type == NULL)
      return;

   // If the types are different then any misuse is likely to generate a
   // type error instead
   if (!type_eq(inner_type, outer_type))
      return;

   diag_t *d = diag_new(DIAG_WARN, tree_loc(inner));
   diag_printf(d, "declaration of %s hides an earlier declaration with the "
               "same type", istr(tree_ident(inner)));
   diag_hint(d, tree_loc(inner), "by the %s declaration here",
             class_str(class_of(inner)));
   diag_hint(d, tree_loc(outer->tree), "this declaration is hidden");
   diag_emit(d);
}

static symbol_t *make_visible(scope_t *s, ident_t name, tree_t decl,
                              visibility_t kind, scope_t *origin)
{
   symbol_t *sym = local_symbol_for(s, name);
   const bool overload = can_overload(decl);
   const tree_kind_t tkind = tree_kind(decl);
   const name_mask_t mask = name_mask_for(decl);
   type_t type = get_type_or_null(decl);

   assert(origin == s || kind != DIRECT);

   if (tkind == T_ATTR_SPEC)
      kind = ATTRIBUTE;
   else if (tkind == T_ALIAS && (tree_flags(decl) & TREE_F_NONOBJECT_ALIAS)) {
      // A nonobject alias points directly to the aliased named entity
      // not to the alias declaration
      decl = tree_ref(tree_value(decl));
   }

   for (int i = 0; i < sym->ndecls; i++) {
      decl_t *dd = get_decl_mutable(sym, i);

      if (dd->tree == decl)
         return sym;
      else if (dd->visibility == HIDDEN || dd->visibility == ATTRIBUTE)
         continue;
      else if (dd->kind == T_LIBRARY) {
         if (tkind == T_LIBRARY && tree_ident(decl) == name)
            return sym;   // Ignore redundant library declarations
         else
            dd->visibility = HIDDEN;
      }
      else if ((!overload || dd->visibility != OVERLOAD) && kind == DIRECT) {
         if (dd->origin == s && is_forward_decl(decl, dd->tree)) {
            // Replace forward declaration in same region with full
            // definition
            dd->visibility = HIDDEN;
         }
         else if (dd->origin == s && s->formal_kind == F_SUBPROGRAM)
            ;   // Resolving subprogram formal names
         else if (dd->visibility == POTENTIAL)
            ;   // Hide declaration visible from use clause
         else if (dd->kind == T_LIBRARY)
            dd->visibility = HIDDEN;    // Library hidden by design unit
         else if (dd->origin == s) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(decl));
            diag_printf(d, "%s already declared in this region", istr(name));
            diag_hint(d, tree_loc(dd->tree), "previous declaration was here");
            diag_hint(d, tree_loc(decl), "duplicate declaration");
            diag_suppress(d, s->suppress);
            diag_emit(d);
            return sym;
         }
         else if (dd->visibility == DIRECT && (dd->mask & mask & N_OBJECT))
            warn_hidden_decl(s, dd, decl);

         dd->visibility = HIDDEN;
      }
      else if (!overload && kind == POTENTIAL && dd->visibility == DIRECT)
         kind = HIDDEN;
      else if (dd->origin == origin && (dd->mask & mask & N_SUBPROGRAM)
               && type_eq(tree_type(dd->tree), type)
               && (tree_flags(dd->tree) & TREE_F_PREDEFINED)) {
         // Allow pre-defined operators be to hidden by user-defined
         // subprograms in the same region
         dd->visibility = HIDDEN;
      }
      else if (is_forward_decl(decl, dd->tree))
         dd->visibility = HIDDEN;
      else if (denotes_same_object(dd->tree, decl) && standard() >= STD_08) {
         // According LRM 08 section 12.3 two declarations are not
         // homographs if they denote different named entities
         return sym;
      }
      else if (overload && kind == DIRECT
               && type_eq(type, tree_type(dd->tree))) {
         if (dd->origin != s) {
            // LRM 93 section 10.3 on visibility specifies that if two
            // declarations are homographs then the one in the inner scope
            // hides the one in the outer scope
            dd->visibility = HIDDEN;
         }
         else if ((dd->kind == T_FUNC_BODY && tkind == T_FUNC_BODY)
                  || (dd->kind == T_PROC_BODY && tkind == T_PROC_BODY)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(decl));
            diag_printf(d, "duplicate subprogram body %s", type_pp(type));
            diag_hint(d, tree_loc(decl), "duplicate definition here");
            diag_hint(d, tree_loc(dd->tree), "previous definition was here");
            diag_suppress(d, s->suppress || type_has_error(type));
            diag_emit(d);
            return sym;
         }
         else if (dd->kind == T_ENUM_LIT && tkind == T_ENUM_LIT) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(decl));
            diag_printf(d, "duplicate enumeration literal %s", istr(sym->name));
            diag_hint(d, tree_loc(decl), "duplicate definition here");
            diag_hint(d, tree_loc(dd->tree), "previous definition was here");
            diag_suppress(d, s->suppress || type_has_error(type));
            diag_emit(d);
            return sym;
         }
         else if (dd->origin == origin) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(decl));
            if (type_strict_eq(tree_type(dd->tree), type))
               diag_printf(d, "%s already declared in this region",
                           type_pp(type));
            else {
               diag_printf(d, "homograph of %s already declared in this region",
                           type_pp(type));
               diag_hint(d, NULL, "only the base type is considered when "
                         "determining if two overloads have the same parameter "
                         "type profile");
            }

            diag_hint(d, tree_loc(dd->tree), "previous declaration was here");
            diag_hint(d, tree_loc(decl), "duplicate declaration");
            diag_suppress(d, s->suppress || type_has_error(type));
            diag_emit(d);
            return sym;
         }
      }
   }

   *add_decl(sym) = (decl_t) {
      .tree       = decl,
      .visibility = overload ? OVERLOAD : kind,
      .origin     = origin,
      .mask       = mask,
      .kind       = tkind,
   };

   sym->mask |= mask;

   if (kind == DIRECT && is_type_decl(decl))
      add_type_literals(s, tree_type(decl), make_visible_slow);

   return sym;
}

static void merge_symbol(scope_t *s, const symbol_t *src)
{
   symbol_t *dst = local_symbol_for(s, src->name);
   dst->mask |= src->mask;

   const bool was_fresh = (dst->ndecls == 0);

   for (int i = 0; i < src->ndecls; i++) {
      const decl_t *dd = get_decl(src, i);
      assert(dd->origin == src->owner);

      if (dd->visibility == HIDDEN || dd->visibility == POTENTIAL)
         continue;

      if (was_fresh) {
         *add_decl(dst) = (decl_t) {
            .tree       = dd->tree,
            .visibility = dd->visibility == DIRECT ? POTENTIAL : dd->visibility,
            .origin     = dd->origin,
            .mask       = dd->mask,
            .kind       = dd->kind,
         };
      }
      else
         make_visible(s, src->name, dd->tree, POTENTIAL, dd->origin);
   }
}

static void merge_scopes(scope_t *to, scope_t *from)
{
   for (sym_chunk_t *chunk = &(from->symbols); chunk; chunk = chunk->chain) {
      for (int i = 0; i < chunk->count; i++) {
         symbol_t *sym = &(chunk->symbols[i]);
         assert(sym->owner->container == from->container);
         merge_symbol(to, sym);
      }
   }
}

static symbol_t *lazy_lib_cb(scope_t *s, ident_t name, void *context)
{
   lib_t lib = context;

   bool error;
   tree_t unit = lib_get_allow_error(lib, name, &error);
   if (unit == NULL || error)
      return NULL;

   return make_visible(s, name, unit, DIRECT, s);
}

static void make_library_visible(scope_t *s, lib_t lib)
{
   lazy_sym_t *l = xmalloc(sizeof(lazy_sym_t));
   l->next = s->lazy;
   l->fn   = lazy_lib_cb;
   l->ctx  = lib;

   s->lazy = l;
}

static void make_visible_slow(scope_t *s, ident_t name, tree_t decl)
{
   // Wrapper for make_visible in the common case of direct visibility
   // and the origin is the same as the target scope

   make_visible(s, name, decl, DIRECT, s);
}

static void make_visible_fast(scope_t *s, ident_t id, tree_t d)
{
   // This should be functionally equivalent to calling make_visible but
   // with the knowledge there are no existing symbols in the scope and
   // it has been checked already

   const tree_kind_t kind = tree_kind(d);
   const name_mask_t mask = name_mask_for(d);

   visibility_t visibility = DIRECT;
   if (can_overload(d))
      visibility = OVERLOAD;
   else if (kind == T_ATTR_SPEC)
      visibility = ATTRIBUTE;

   symbol_t *sym = local_symbol_for(s, id);
   *add_decl(sym) = (decl_t) {
      .tree       = d,
      .visibility = visibility,
      .origin     = s,
      .mask       = mask,
      .kind       = kind,
   };

   sym->mask |= mask;

   if (is_type_decl(d))
      add_type_literals(s, tree_type(d), make_visible_fast);
}

static void make_potentially_visible(scope_t *s, ident_t id, tree_t d)
{
   make_visible(s, id, d, POTENTIAL, s);
}

static ident_t unit_bare_name(tree_t unit)
{
   ident_t unit_name = tree_ident(unit);
   return ident_rfrom(unit_name, '.') ?: unit_name;
}

static scope_t *scope_for_type(nametab_t *tab, type_t type)
{
   assert(type_is_protected(type));

   void *key = type;

   scope_t *s = hash_get(tab->globalmap, key);
   if (s != NULL)
      return s;

   s = xcalloc(sizeof(scope_t));
   s->lookup   = hash_new(128);
   s->sym_tail = &(s->symbols);
   s->chain    = tab->globals;

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t d = type_field(type, i);
      make_visible_fast(s, tree_ident(d), d);
   }

   hash_put(tab->globalmap, key, s);
   return (tab->globals = s);
}

static scope_t *private_scope_for(nametab_t *tab, tree_t unit)
{
   const tree_kind_t kind = tree_kind(unit);

   hash_t *cache = tab->globalmap;
   ident_t id = tree_ident(unit);
   void *key = unit;
   if (kind == T_LIBRARY)
      key = id;   // Tree pointer is not stable
   else if (is_well_known(id) < NUM_WELL_KNOWN) {
      // Standard packages can be safely stored in a global cache as we
      // know they won't change
      static hash_t *stdcache = NULL;
      INIT_ONCE(stdcache = hash_new(64));
      cache = stdcache;
   }

   scope_t *s = hash_get(cache, key);
   if (s != NULL)
      return s;

   s = xcalloc(sizeof(scope_t));
   s->lookup    = hash_new(128);
   s->sym_tail  = &(s->symbols);
   s->container = unit;

   if (cache == tab->globalmap) {
      s->chain = tab->globals;
      tab->globals = s;
   }

   if (kind == T_LIBRARY)
      make_library_visible(s, lib_require(tree_ident(unit)));
   else {
      // For package instances do not export the names declared only in
      // the body
      const int ndecls = kind == T_PACK_INST && tree_has_ref(unit)
         ? tree_decls(tree_ref(unit))
         : tree_decls(unit);

      for (int i = 0; i < ndecls; i++) {
         tree_t d = tree_decl(unit, i);
         const tree_kind_t dkind = tree_kind(d);
         if (dkind == T_ALIAS || dkind == T_TYPE_DECL) {
            // Handle special cases: an alias may make the same
            // enumeration literal visible multiple times and a type
            // declaration may hide an earlier incomplete type
            make_visible_slow(s, tree_ident(d), d);
         }
         else if (dkind != T_USE)
            make_visible_fast(s, tree_ident(d), d);
      }

      switch (kind) {
      case T_ENTITY:
      case T_BLOCK:
         {
            const int nports = tree_ports(unit);
            for (int i = 0; i < nports; i++) {
               tree_t p = tree_port(unit, i);
               make_visible_fast(s, tree_ident(p), p);
            }
         }
         // Fall-through
      case T_PACKAGE:
      case T_PACK_INST:
         {
            const int ngenerics = tree_generics(unit);
            for (int i = 0; i < ngenerics; i++) {
               tree_t g = tree_generic(unit, i), decl = g;

               const class_t class = tree_class(g);
               if (class == C_FUNCTION || class == C_PROCEDURE) {
                  // A single subprogram could be visible both directly
                  // and as a actual generic subprogram
                  tree_t value = find_generic_map(unit, i, g);
                  if (value == NULL)
                     assert(error_count() > 0
                            || is_uninstantiated_package(unit));
                  else if (tree_kind(value) == T_REF && tree_has_ref(value)) {
                     decl = tree_ref(value);
                     assert(is_subprogram(decl));
                  }
               }

               make_visible_fast(s, tree_ident(g), decl);
            }
         }
         break;

      default:
         break;
      }

      ident_t bare_name = unit_bare_name(unit);
      if (symbol_for(s, bare_name) == NULL)
         make_visible_fast(s, bare_name, unit);
   }

   hash_put(cache, key, s);
   return s;
}

static bool is_forward_decl(tree_t decl, tree_t existing)
{
   tree_kind_t tkind = tree_kind(decl);
   tree_kind_t ekind = tree_kind(existing);

   if ((tkind == T_TYPE_DECL || tkind == T_PROT_DECL) && ekind == T_TYPE_DECL)
      return type_kind(tree_type(existing)) == T_INCOMPLETE;
   else if ((tkind == T_FUNC_BODY && ekind == T_FUNC_DECL)
            || (tkind == T_PROC_BODY && ekind == T_PROC_DECL)) {
      return type_eq(tree_type(decl), tree_type(existing))
         && !(tree_flags(existing) & TREE_F_PREDEFINED);
   }
   else if (tkind == T_PROT_BODY && ekind == T_PROT_DECL)
      return type_eq(tree_type(decl), tree_type(existing));
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

   const symbol_t *sym = hash_get(region->lookup, tree_ident(decl));
   if (sym == NULL)
      return NULL;

   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->origin == region && is_forward_decl(decl, dd->tree))
         return dd->tree;
   }

   return NULL;
}

tree_t get_local_decl(nametab_t *tab, tree_t container, ident_t name, int nth)
{
   scope_t *scope;
   if (container == NULL || container == tab->top_scope->container)
      scope = tab->top_scope;
   else
      scope = private_scope_for(tab, container);

   const symbol_t *sym = hash_get(scope->lookup, name);
   if (sym == NULL)
      return NULL;

   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->origin != scope || dd->visibility == HIDDEN)
         continue;
      else if (nth-- == 0)
         return dd->tree;
   }

   return NULL;
}

bool is_same_region(nametab_t *tab, tree_t decl)
{
   const symbol_t *sym = hash_get(tab->top_scope->lookup, tree_ident(decl));
   if (sym == NULL)
      return false;

   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->origin == tab->top_scope && dd->tree == decl)
         return true;
   }

   return false;
}

psl_node_t find_default_clock(nametab_t *tab)
{
   ident_t id = well_known(W_DEFAULT_CLOCK);

   const symbol_t *sym = symbol_for(tab->top_scope, id);
   if (sym == NULL)
      return NULL;

   const decl_t *dd = get_decl(sym, 0);
   assert(dd->kind == T_PSL);
   return tree_psl(dd->tree);
}

static bool is_predef_for_type(tree_t d, type_t type)
{
   if (!is_subprogram(d))
      return false;
   else if (!is_operator_symbol(tree_ident(d))
            && !(tree_flags(d) & TREE_F_PREDEFINED))
      return false;
   else if (tree_ports(d) == 0)
      return false;
   else
      return type_strict_eq(tree_type(tree_port(d, 0)), type);
}

void walk_predefs(nametab_t *tab, ident_t name, predef_cb_t fn, void *context)
{
   const symbol_t *sym = iterate_symbol_for(tab, name);
   assert(sym != NULL);

   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (!is_type_decl(dd->tree))
         continue;

      tree_t region = NULL;
      for (scope_t *s = dd->origin;
           (region = s->container) == NULL;
           s = s->parent)
         ;

      type_t type = tree_type(dd->tree);

      const int ndecls = tree_decls(region);
      for (int i = 0; i < ndecls; i++) {
         tree_t d = tree_decl(region, i);
         if (is_predef_for_type(d, type))
            (*fn)(d, context);
      }
   }
}

void insert_name(nametab_t *tab, tree_t decl, ident_t alias)
{
   make_visible_slow(tab->top_scope, alias ?: tree_ident(decl), decl);
}

void hide_name(nametab_t *tab, ident_t name)
{
   const symbol_t *exist = symbol_for(tab->top_scope, name);
   if (exist == NULL || exist->owner == tab->top_scope)
      return;
   else if (opt_get_int(OPT_RELAXED))
      return;

   symbol_t *sym = local_symbol_for(tab->top_scope, name);

   for (int i = 0; i < sym->ndecls; i++) {
      decl_t *dd = get_decl_mutable(sym, i);

      if (dd->origin != tab->top_scope && dd->visibility == DIRECT)
         dd->visibility = HIDDEN;
   }
}

void insert_spec(nametab_t *tab, tree_t spec, spec_kind_t kind,
                 ident_t ident, int depth)
{
   scope_t *scope;
   for (scope = tab->top_scope; depth > 0; depth--, scope = scope->parent)
      ;
   assert(scope != NULL);

   spec_t **p;
   for (p = &(scope->specs); *p != NULL; p = &((*p)->next)) {
      if (kind == SPEC_EXACT && (*p)->ident == ident) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(spec));
         diag_printf(d, "duplicate specification for instance %s", istr(ident));
         diag_hint(d, tree_loc((*p)->tree), "previous specification was here");
         diag_hint(d, tree_loc(spec), "duplicate specification here");
         diag_emit(d);
         return;
      }
   }

   spec_t *s = xmalloc(sizeof(spec_t));
   s->next    = NULL;
   s->kind    = kind;
   s->ident   = ident;
   s->tree    = spec;
   s->matches = 0;

   *p = s;
}

void continue_proc_labelling_from(tree_t t, nametab_t *tab)
{
   if (t == NULL) {
      tab->top_scope->lbl_cnts.proc = 0;
      return;
   }

   assert (tree_kind(t) == T_ENTITY);

   const int nstmts = tree_stmts(t);
   int cnt = 0;
   ident_t prefix = ident_new("_P");
   for (int i = 0; i < nstmts; i++) {
      tree_t stmt = tree_stmt(t, i);

      // Entity statements for sure have labels since they have been implicitly
      // labelled
      if (ident_starts_with(tree_ident(stmt), prefix))
         cnt++;
   }
   tab->top_scope->lbl_cnts.proc = cnt;
}

ident_t get_implicit_label(tree_t t, nametab_t *tab)
{
   int *cnt;
   char c;
   char buf[22];

   switch (tree_kind(t)) {
   case T_CONCURRENT:
   case T_PROCESS:
   case T_PSL:
      cnt = &(tab->top_scope->lbl_cnts.proc);
      c = 'P';
      break;

   case T_FOR:
   case T_WHILE:
   case T_LOOP:
      cnt = NULL;
      for (scope_t *s = tab->top_scope; s != NULL; s = s->parent) {
         if (s->container == NULL)
            continue;
         if (is_subprogram(s->container) || tree_kind(s->container) == T_PROCESS) {
            cnt = &(s->lbl_cnts.loop);
            break;
         }
      }
      c = 'L';
      break;

   default:
      return NULL;
   }

   checked_sprintf(buf, sizeof(buf), "_%c%d", c, (*cnt)++);
   return ident_new(buf);
}

type_t resolve_type(nametab_t *tab, type_t incomplete)
{
   assert(type_kind(incomplete) == T_INCOMPLETE);

   ident_t name = ident_rfrom(type_ident(incomplete), '.');

   const symbol_t *sym = symbol_for(tab->top_scope, name);
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->kind == T_TYPE_DECL || dd->kind == T_PROT_DECL) {
            type_t def = tree_type(dd->tree);
            if (type_kind(def) != T_INCOMPLETE && type_eq(def, incomplete))
               return def;
         }
      }
   }

   return incomplete;
}

name_mask_t query_name(nametab_t *tab, ident_t name, tree_t *p_decl)
{
   const symbol_t *sym = iterate_symbol_for(tab, name);
   if (sym == NULL)
      return 0;

   name_mask_t mask = 0;
   tree_t uniq = NULL;
   int count = 0;
   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->visibility != HIDDEN && dd->visibility != ATTRIBUTE) {
         count++;
         uniq = dd->tree;
         mask |= name_mask_for(dd->tree);
      }
   }

   if (p_decl && count == 1) *p_decl = uniq;

   mask |= count << 16;
   return mask;
}

tree_t query_spec(nametab_t *tab, tree_t object)
{
   if (!tree_has_ref(object))
      return NULL;   // Was earlier error

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

static bool denotes_same_object(tree_t a, tree_t b)
{
   if (tree_kind(a) == T_ALIAS) {
      tree_t value = tree_value(a);
      if (tree_kind(value) == T_REF && tree_has_ref(value))
         return denotes_same_object(tree_ref(value), b);
   }
   else if (tree_kind(b) == T_ALIAS) {
      tree_t value = tree_value(b);
      if (tree_kind(value) == T_REF && tree_has_ref(value))
         return denotes_same_object(a, tree_ref(value));
   }

   return a == b;
}

static const symbol_t *iterate_symbol_for(nametab_t *tab, ident_t name)
{
   for (scope_t *s = tab->top_scope, *ss = NULL; ; s = ss, ss = NULL) {
      ident_t next = ident_walk_selected(&name);
      const symbol_t *sym = lazy_symbol_for(s, next);
      if (sym == NULL || name == NULL)
         return sym;

      for (int i = 0; i < sym->ndecls && ss == NULL; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (dd->kind == T_LIBRARY || is_container(dd->tree))
            ss = private_scope_for(tab, dd->tree);
      }

      if (ss == NULL) return NULL;
   }
}

static void hint_for_typo(scope_t *top_scope, diag_t *d, ident_t name,
                          name_mask_t filter)
{
   if (ident_runtil(name, '.') != name)
      return;   // Ignore selected names

   const symbol_t *best = NULL;
   int bestd = INT_MAX;

   for (scope_t *s = top_scope; s != NULL; s = s->parent) {
      for (sym_chunk_t *chunk = &(s->symbols); chunk; chunk = chunk->chain) {
         for (int i = 0; i < chunk->count; i++) {
            if (chunk->symbols[i].mask & filter) {
               const int d = ident_distance(chunk->symbols[i].name, name);
               if (d > 0 && d < bestd) {
                  best = &(chunk->symbols[i]);
                  bestd = d;
               }
            }
         }
      }
   }

   if (bestd <= (ident_len(name) <= 4 ? 2 : 3))
      diag_hint(d, diag_get_loc(d), "did you mean %s?", istr(best->name));
}

static type_t get_result_type(nametab_t *tab, tree_t decl)
{
   type_t type = tree_type(decl);
   if (type_kind(type) != T_FUNC)
      return type;

   type_t rtype = type_result(type);
   if (type_is_incomplete(rtype))
      return resolve_type(tab, rtype);

   return rtype;
}

static tree_t get_aliased_subprogram(tree_t t)
{
   assert(tree_kind(t) == T_ALIAS);
   tree_t aliased = tree_value(t);
   const tree_kind_t kind = tree_kind(aliased);
   if (kind == T_REF && tree_has_ref(aliased)) {
      tree_t decl = tree_ref(aliased);
      if (tree_kind(decl) == T_ALIAS)
         return get_aliased_subprogram(decl);
      else
         return decl;
   }
   else if (kind == T_PROT_REF)
      return aliased;
   else
      return NULL;
}

static bool can_call_no_args(tree_t t)
{
   switch (tree_kind(t)) {
   case T_FUNC_DECL:
   case T_FUNC_BODY:
   case T_FUNC_INST:
   case T_PROC_DECL:
   case T_PROC_BODY:
   case T_PROC_INST:
   case T_GENERIC_DECL:
      return !!(tree_flags(t) & TREE_F_CALL_NO_ARGS);
   case T_ALIAS:
      return can_call_no_args(get_aliased_subprogram(t));
   case T_PROT_REF:
      return can_call_no_args(tree_ref(t));
   default:
      return false;
   }
}

static tree_t try_resolve_name(nametab_t *tab, ident_t name)
{
   const symbol_t *sym = iterate_symbol_for(tab, name);
   if (sym == NULL)
      return NULL;

   if (sym->ndecls == 1) {
      const decl_t *dd = get_decl(sym, 0);
      if (dd->visibility != HIDDEN)
         return dd->tree;
   }

   // Check if all but one declaration is hidden
   int hidden = 0, overload = 0, subprograms = 0;
   tree_t result = NULL;
   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->visibility == HIDDEN || dd->visibility == ATTRIBUTE)
         hidden++;
      else
         result = dd->tree;

      if (dd->visibility == OVERLOAD) {
         overload++;
         if (dd->mask & N_SUBPROGRAM)
            subprograms++;
      }
   }

   if (hidden == sym->ndecls - 1)
      return result;
   else if (overload == 0 || tab->top_type_set == NULL)
      return NULL;

   // Use the context to determine the correct overload

   SCOPED_A(tree_t) m = AINIT;
   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->visibility == OVERLOAD || tab->top_type_set->members.count == 0)
         APUSH(m, dd->tree);
   }

   if (m.count > 1 && subprograms > 0) {
      unsigned wptr = 0;
      for (int i = 0; i < m.count; i++) {
         if (is_subprogram(m.items[i])) {
            // Remove subprograms that cannot be called with zero
            // arguments
            if (!can_call_no_args(m.items[i]))
               continue;

            // Remove subprograms where the result does not satisfy the
            // required predicate
            type_pred_t p = tab->top_type_set->pred;
            if (p != NULL && !(*p)(get_result_type(tab, m.items[i])))
               continue;
         }

         m.items[wptr++] = m.items[i];
      }
      ATRIM(m, wptr);
   }

   if (m.count > 1) {
      // Remove any duplicates from aliases
      unsigned wptr = 0;
      for (int i = 0; i < m.count; i++) {
         bool is_dup = false;
         for (int j = 0; !is_dup && j < i; j++) {
            if (denotes_same_object(m.items[i], m.items[j]))
               is_dup = true;
         }

         if (!is_dup)
            m.items[wptr++] = m.items[i];
      }
      assert(wptr >= 1);
      ATRIM(m, wptr);
   }

   if (m.count == 1)
      return m.items[0];

   if (tab->top_type_set == NULL)
      return NULL;

   const bool type_set_empty = tab->top_type_set->members.count == 0;

   unsigned wptr = 0;
   for (unsigned i = 0; i < m.count; i++) {
      if (class_has_type(class_of(m.items[i]))) {
         type_t type = get_result_type(tab, m.items[i]);
         if (type_set_empty)
            type_set_add(tab, type, m.items[i]);
         else if (type_set_contains(tab, type))
            m.items[wptr++] = m.items[i];
      }
   }
   ATRIM(m, wptr);

   if (m.count == 1)
      return m.items[0];

   return NULL;
}

tree_t resolve_name(nametab_t *tab, const loc_t *loc, ident_t name)
{
   tree_t decl = try_resolve_name(tab, name);
   if (decl != NULL)
      return decl;

   const symbol_t *sym = iterate_symbol_for(tab, name);
   if (sym == NULL) {
      if (tab->top_scope->suppress) {
         // Suppressing cascading errors
         assert(error_count() > 0);
      }
      else if (name == well_known(W_ERROR)) {
         // Was a parse error, suppress further errors
      }
      else if (tab->top_scope->formal_kind != F_NONE
               && tab->top_scope->formal_fn != NULL) {
         diag_t *d = diag_new(DIAG_ERROR, loc);

         if (tab->top_scope->formal_fn != NULL)
            (*tab->top_scope->formal_fn)(d, name, tab->top_scope->formal_arg);

         diag_emit(d);
         tab->top_scope->formal_fn = NULL;  // Suppress further errors
      }
      else if (tab->top_scope->overload && !tab->top_scope->overload->error) {
         diag_t *d = diag_new(DIAG_ERROR, loc);
         diag_printf(d, "no possible overload of %s has formal %s",
                     istr(tab->top_scope->overload->name), istr(name));
         diag_hint(d, loc, "formal argument name %s", istr(name));
         hint_for_typo(tab->top_scope, d, name, N_OBJECT);

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
               diag_hint(d, tree_loc(t), "%s", tb_get(tb));
            }
         }

         diag_emit(d);
         tab->top_scope->overload->error = true;
      }
      else if (tab->top_scope->formal_kind != F_NONE)
         ;  // Was earlier error
      else if (tab->top_scope->overload == NULL) {
         diag_t *d = diag_new(DIAG_ERROR, loc);

         ident_t prefix = ident_runtil(name, '.');
         const symbol_t *psym = prefix ? iterate_symbol_for(tab, prefix) : NULL;
         if (psym != NULL && psym->ndecls == 1) {
            if (psym->decls[0].kind == T_LIBRARY) {
               lib_t lib = lib_require(psym->name);
               if (lib_had_errors(lib, name)) {
                  diag_printf(d, "design unit depends on %s which was analysed"
                              " with errors", istr(name));
                  tab->top_scope->suppress = true;
               }
               else
                  diag_printf(d, "design unit %s not found in library %s",
                              istr(ident_rfrom(name, '.')), istr(psym->name));
            }
            else
               diag_printf(d, "name %s not found in %s %s",
                           istr(ident_rfrom(name, '.')),
                           (is_design_unit(psym->decls[0].tree)
                            ? "design unit" : "object"),
                           istr(psym->name));
         }
         else
            diag_printf(d, "no visible declaration for %s", istr(name));

         hint_for_typo(tab->top_scope, d, name, N_OBJECT | N_TYPE);
         diag_emit(d);
      }

      // Suppress further errors for this name
      local_symbol_for(tab->top_scope, name)->mask |= N_ERROR;

      return NULL;
   }

   int hidden = 0, overload = 0;
   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->visibility == HIDDEN || dd->visibility == ATTRIBUTE)
         hidden++;
      else if (dd->visibility == OVERLOAD)
         overload++;
   }

   if (type_set_any(tab, type_is_none))
      return NULL;  // Suppress cascading errors
   else if ((sym->mask & N_ERROR) || tab->top_scope->suppress)
      return NULL;    // Was an earlier error

   tree_kind_t what = T_LAST_TREE_KIND;
   for (unsigned i = 0; i < sym->ndecls; i++) {
      const tree_kind_t kind = tree_kind(get_decl(sym, i)->tree);
      if (what == T_LAST_TREE_KIND)
         what = kind;
      else if (what != kind)
         what = T_REF;
   }

   diag_t *d = diag_new(DIAG_ERROR, loc);
   if (hidden > 0 && hidden == sym->ndecls) {
      diag_printf(d, "declaration of %s is hidden", istr(name));
      diag_hint(d, NULL, "the $bold$--relaxed$$ option suppresses this error");
   }
   else if (overload == 0)
      diag_printf(d, "multiple conflicting visible declarations of %s",
                  istr(name));
   else
      diag_printf(d, "ambiguous use of %s %s",
                  what == T_ENUM_LIT ? "enumeration literal"
                  : (what == T_UNIT_DECL ? "physical literal" : "name"),
                  istr(name));

   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      LOCAL_TEXT_BUF tb = tb_new();

      switch (dd->visibility) {
      case HIDDEN:    tb_cat(tb, "hidden"); break;
      case OVERLOAD:  tb_cat(tb, "visible"); break;
      case DIRECT:    tb_cat(tb, "directly visible"); break;
      case POTENTIAL: tb_cat(tb, "potentially visible"); break;
      case ATTRIBUTE: continue;
      }
      tb_printf(tb, " declaration of %s", istr(name));

      type_t type = get_type_or_null(dd->tree);
      if (type != NULL && !is_type_decl(dd->tree))
         tb_printf(tb, " as %s", type_pp(tree_type(dd->tree)));

      const bool has_named_origin =
         dd->origin->container != NULL
         && (tree_kind(dd->origin->container) != T_PROCESS
             || !(tree_flags(dd->origin->container) & TREE_F_SYNTHETIC_NAME));

      if (has_named_origin)
         tb_printf(tb, " from %s", istr(tree_ident(dd->origin->container)));

      diag_hint(d, tree_loc(dd->tree), "%s", tb_get(tb));
   }

   diag_hint(d, loc, "use of name %s here", istr(name));
   diag_emit(d);

   return NULL;
}

static tree_t resolve_method_name(nametab_t *tab, tree_t ref, type_t constraint)
{
   assert(tree_kind(ref) == T_PROT_REF);
   assert(constraint != NULL);

   type_t ptype = tree_type(tree_value(ref));
   assert(type_is_protected(ptype));

   scope_t *scope = scope_for_type(tab, ptype);
   const symbol_t *sym = symbol_for(scope, tree_ident(ref));

   SCOPED_A(tree_t) matching = AINIT;
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (dd->mask & N_SUBPROGRAM) {
            type_t signature = tree_type(dd->tree);
            if (type_eq_map(constraint, signature, tab->top_scope->gmap))
               APUSH(matching, dd->tree);
         }
      }
   }

   assert(matching.count <= 1);

   if (matching.count == 1)
      return matching.items[0];
   else if (tab->top_scope->suppress)
      return NULL;
   else {
      const char *signature = strchr(type_pp(constraint), '[');
      error_at(tree_loc(ref), "no visible method %s in protected type %s "
               "matches signature %s", istr(tree_ident(ref)), type_pp(ptype),
               signature);
      return NULL;
   }
}

tree_t resolve_uninstantiated_subprogram(nametab_t *tab, const loc_t *loc,
                                         ident_t name, type_t constraint)
{
   const symbol_t *sym = iterate_symbol_for(tab, name);

   SCOPED_A(tree_t) m = AINIT;
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (dd->mask & N_SUBPROGRAM) {
            if (constraint == NULL)
               APUSH(m, dd->tree);
            else {
               type_t signature = tree_type(dd->tree);
               if (type_eq_map(constraint, signature, tab->top_scope->gmap))
                  APUSH(m, dd->tree);
            }
         }
      }
   }

   if (m.count > 1) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < m.count; i++) {
         if (is_uninstantiated_subprogram(m.items[i]))
            m.items[wptr++] = m.items[i];
      }
      ATRIM(m, wptr);
   }

   if (m.count == 1) {
      if (is_uninstantiated_subprogram(m.items[0]))
         return m.items[0];
      else {
         error_at(loc, "%s %s is not an uninstantiated subprogram",
                  class_str(class_of(m.items[0])), istr(name));
         return NULL;
      }
   }
   else if (tab->top_scope->suppress)
      return NULL;
   else if (constraint != NULL) {
      const char *signature = strchr(type_pp(constraint), '[');
      error_at(loc, "no visible uninstantiated subprogram %s matches "
               "signature %s", istr(name), signature);
      return NULL;
   }
   else if (m.count == 0) {
      error_at(loc,"no visible uninstantiated subprogram declaration for %s",
               istr(name));
      return NULL;
   }
   else {
      diag_t *d = diag_new(DIAG_ERROR, loc);
      diag_printf(d, "multiple visible uninstantiated subprograms with "
                  "name %s", istr(name));
      for (int i = 0; i < m.count; i++)
         diag_hint(d, tree_loc(m.items[i]), "visible declaration of %s",
                   type_pp(tree_type(m.items[i])));
      diag_hint(d, loc, "use of name %s here", istr(name));
      diag_hint(d, NULL, "use an explicit subprogram signature to select "
                "a particular overload");
      diag_emit(d);
      return NULL;
   }
}

tree_t resolve_subprogram_name(nametab_t *tab, const loc_t *loc, ident_t name,
                               type_t constraint)
{
   const bool allow_enum =
      type_kind(constraint) == T_FUNC && type_params(constraint) == 0;

   const symbol_t *sym = iterate_symbol_for(tab, name);

   SCOPED_A(tree_t) matching = AINIT;
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (dd->mask & N_SUBPROGRAM) {
            type_t signature = tree_type(dd->tree);
            if (type_eq_map(constraint, signature, tab->top_scope->gmap))
               APUSH(matching, dd->tree);
         }
         else if (allow_enum && dd->kind == T_ENUM_LIT
                  && type_eq(type_result(constraint), tree_type(dd->tree)))
            APUSH(matching, dd->tree);
      }
   }

   if (matching.count == 1)
      return matching.items[0];
   else if (tab->top_scope->suppress)
      return NULL;
   else {
      const char *signature = strchr(type_pp(constraint), '[');
      error_at(loc, "no visible subprogram%s %s matches signature %s",
               allow_enum ? " or enumeration literal" : "",
               istr(name), signature);
      return NULL;
   }
}

static tree_t resolve_ref(nametab_t *tab, tree_t ref)
{
   const loc_t *loc = tree_loc(ref);
   ident_t name = tree_ident(ref);

   type_t constraint;
   if (type_set_uniq(tab, &constraint) && type_is_subprogram(constraint)) {
      // Reference to subprogram or enumeration literal with signature
      return resolve_subprogram_name(tab, loc, name, constraint);
   }
   else {
      // Ordinary reference
      return resolve_name(tab, loc, name);
   }
}

void resolve_resolution(nametab_t *tab, tree_t rname, type_t type)
{
   // Finding the resolution function is a special case of overload resolution

   if (tree_kind(rname) == T_ELEM_RESOLUTION) {
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
      type_set_add(tab, type, NULL);

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

void insert_names_from_use(nametab_t *tab, tree_t use)
{
   assert(tree_kind(use) == T_USE);

   if (!tree_has_ref(use) || !tree_has_ident2(use))
      return;   // Was earlier error

   tree_t unit = tree_ref(use);
   ident_t what = tree_ident2(use);

   if (tree_kind(unit) == T_GENERIC_DECL) {
      assert(tree_class(unit) == C_PACKAGE);

      tree_t map = tree_value(unit);
      assert(tree_kind(map) == T_PACKAGE_MAP);

      unit = tree_ref(map);
      assert(is_uninstantiated_package(unit));
   }

   if (tree_kind(unit) == T_LIBRARY) {
      ident_t lib_name = tree_ident2(unit);
      ident_t unit_name = ident_prefix(lib_name, what, '.');
      lib_t lib = lib_require(lib_name);
      bool error;
      if (what == well_known(W_ALL)) {
         make_library_visible(tab->top_scope, lib);
         return;
      }
      else if ((unit = lib_get_allow_error(lib, unit_name, &error)) == NULL) {
         error_at(tree_loc(use), "design unit %s not found in library %s",
                  istr(what), istr(lib_name));
         return;
      }
      else if (error) {
         error_at(tree_loc(use), "design unit %s was analysed with errors",
                  istr(unit_name));
         return;
      }

      make_visible(tab->top_scope, what, unit, POTENTIAL, tab->top_scope);
      return;
   }

   assert(is_package(unit));

   scope_t *s = private_scope_for(tab, unit);
   assert(s->container == unit);

   if (what == well_known(W_ALL)) {
      for (int i = 0; i < tab->top_scope->imported.count; i++) {
         if (tab->top_scope->imported.items[i] == unit)
            return;
      }

      merge_scopes(tab->top_scope, s);
      APUSH(tab->top_scope->imported, unit);
   }
   else {
      const symbol_t *sym = symbol_for(s, what);
      if (sym == NULL) {
         error_at(tree_loc(use), "name %s not found in %s %s",
                  istr(what), is_design_unit(unit) ? "design unit" : "object",
                  istr(tree_ident(unit)));
         return;
      }

      merge_symbol(tab->top_scope, sym);

      if (standard() >= STD_08) {
         // If this is an enumeration or physical type then also make
         // the literals and operators visible
         for (int i = 0; i < sym->ndecls; i++) {
            const decl_t *dd = get_decl(sym, i);
            if (!is_type_decl(dd->tree))
               continue;

            type_t type = type_base_recur(tree_type(dd->tree));
            add_type_literals(tab->top_scope, type, make_potentially_visible);

            const int ndecls = tree_decls(unit);
            for (int i = 0; i < ndecls; i++) {
               tree_t d = tree_decl(unit, i);
               if (is_predef_for_type(d, type))
                  make_visible(tab->top_scope, tree_ident(d), d,
                               POTENTIAL, s);
            }
         }
      }

      merge_symbol(tab->top_scope, symbol_for(s, unit_bare_name(unit)));
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
         insert_name(tab, d, NULL);
   }
}

void insert_ports(nametab_t *tab, tree_t container)
{
   const int nports = tree_ports(container);
   for (int i = 0; i < nports; i++)
      insert_name(tab, tree_port(container, i), NULL);
}

void insert_generics(nametab_t *tab, tree_t container)
{
   const int ngenerics = tree_generics(container);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(container, i);
      make_visible_slow(tab->top_scope, tree_ident(g), g);
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
         insert_name(tab, c, NULL);
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

void insert_names_for_psl(nametab_t *tab)
{
   if (tab->psl == NULL) {
      tree_t nvc = tree_new(T_LIBRARY);
      ident_t nvc_i = well_known(W_NVC);
      tree_set_ident(nvc, nvc_i);
      tree_set_ident2(nvc, nvc_i);
      insert_name(tab, nvc, nvc_i);

      ident_t psl_support_i = well_known(W_NVC_PSL_SUPPORT);
      lib_t lnvc = lib_require(nvc_i);
      tree_t psl_support = lib_get(lnvc, psl_support_i);
      if (psl_support == NULL)
         fatal("required package %s not found", istr(psl_support_i));

      tree_t u = tree_new(T_USE);
      tree_set_ident(u, psl_support_i);
      tree_set_ident2(u, well_known(W_ALL));
      tree_set_ref(u, psl_support);

      tab->psl = u;
   }

   insert_names_from_use(tab, tab->psl);
}

static void missing_record_field_cb(diag_t *d, ident_t id, void *arg)
{
   diag_printf(d, "record type %s has no field named %s",
               type_pp((type_t)arg), istr(id));
}

void push_scope_for_fields(nametab_t *tab, type_t type)
{
   assert(type_is_record(type));

   push_scope(tab);

   tab->top_scope->formal_kind = F_RECORD;
   tab->top_scope->formal_fn   = missing_record_field_cb;
   tab->top_scope->formal_arg  = type;

   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++)
      insert_name(tab, type_field(type, i), NULL);
}

static void missing_generic_cb(diag_t *d, ident_t id, void *arg)
{
   diag_printf(d, "%s has no generic named %s",
               istr(tree_ident((tree_t)arg)), istr(id));
}

static void missing_port_cb(diag_t *d, ident_t id, void *arg)
{
   tree_t unit = arg;

   diag_printf(d, "%s has no port named %s",
               istr(tree_ident(arg)), istr(id));
   diag_hint(d, diag_get_loc(d), "use of name %s in port map", istr(id));

   const int nports = tree_ports(unit);
   if (nports > 0) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_printf(tb, "%s %s has port%s ",
                tree_kind(unit) == T_COMPONENT ? "component" : "entity",
                istr(tree_ident(unit)), nports > 1 ? "s" : "");
      for (int j = 0; j < nports; j++) {
         if (j > 0 && j == nports - 1)
            tb_cat(tb, " and ");
         else if (j > 0)
            tb_cat(tb, ", ");
         tb_cat(tb, istr(tree_ident(tree_port(unit, j))));
      }
      diag_hint(d, tree_loc(unit), "%s", tb_get(tb));
   }
}

void push_scope_for_formals(nametab_t *tab, formal_kind_t kind, tree_t unit)
{
   push_scope(tab);

   tab->top_scope->formal_kind = kind;
   tab->top_scope->formal_arg  = unit;

   if (unit != NULL) {
      switch (kind) {
      case F_GENERIC_MAP:
         tab->top_scope->formal_fn = missing_generic_cb;
         insert_generics(tab, unit);
         break;
      case F_PORT_MAP:
         tab->top_scope->formal_fn = missing_port_cb;
         insert_ports(tab, unit);
         break;
      case F_SUBPROGRAM:
         break;
      case F_RECORD:
      case F_NONE:
         fatal_trace("invalid kind in push_scope_for_formals");
         break;
      }
   }
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
         insert_name(tab, d, NULL);
   }

   const int nstmts = tree_stmts(unit);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(unit, i);
      insert_name(tab, s, NULL);
   }
}

////////////////////////////////////////////////////////////////////////////////
// Name mangling

void mangle_func(nametab_t *tab, tree_t decl)
{
   if (tree_has_ident2(decl))
      return;

   LOCAL_TEXT_BUF buf = tb_new();
   tb_istr(buf, tab->top_scope->prefix);
   tb_append(buf, '.');
   tb_istr(buf, tree_ident(decl));

   const tree_kind_t kind = tree_kind(decl);
   const bool is_func = kind == T_FUNC_BODY || kind == T_FUNC_DECL
      || kind == T_FUNC_INST;
   const int nports = tree_ports(decl);
   if (nports > 0 || is_func)
      tb_append(buf, '(');

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(decl, i);
      if (tree_class(p) == C_SIGNAL)
         tb_append(buf, 's');
      mangle_one_type(buf, tree_type(p));
   }

   if (nports > 0 || is_func)
      tb_append(buf, ')');

   if (is_func)
      mangle_one_type(buf, type_result(tree_type(decl)));

   if (tree_flags(decl) & TREE_F_PREDEFINED)
      tb_cat(buf, "$predef");

   tree_set_ident2(decl, ident_new(tb_get(buf)));
}

void mangle_type(nametab_t *tab, type_t type)
{
   ident_t id = ident_prefix(tab->top_scope->prefix, type_ident(type), '.');
   type_set_ident(type, id);
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
      if (o->candidates.items[i] == d)
         return;   // Duplicate from alias
      else if (tree_kind(o->candidates.items[i]) != skind)
         continue;
      else if (type_eq(tree_type(o->candidates.items[i]), dtype)) {
         if (prefer) o->candidates.items[i] = d;
         return;
      }
   }

   APUSH(o->candidates, d);
}

static type_t get_protected_type(nametab_t *tab, tree_t t)
{
   switch (tree_kind(t)) {
   case T_ALL:
   case T_ALIAS:
      return get_protected_type(tab, tree_value(t));
   case T_REF:
      return get_protected_type(tab, tree_ref(t));
   case T_VAR_DECL:
   case T_PARAM_DECL:
   case T_PORT_DECL:
   case T_ATTR_REF:
   case T_FCALL:
   case T_PROT_FCALL:
      {
         type_t type = tree_type(t);
         for (;;) {
            if (type_is_access(type))
               type = type_designated(type);
            else if (type_is_protected(type))
               return type;
            else if (type_is_incomplete(type)) {
               type_t next = resolve_type(tab, type);
               if (next == type)
                  return NULL;

               type = next;
            }
            else
               return NULL;
         }
      }
   default:
      return NULL;
   }
}

static void begin_overload_resolution(overload_t *o)
{
   if (o->prefix != NULL) {
      type_t type = get_protected_type(o->nametab, o->prefix);
      if (type != NULL) {
         scope_t *scope = scope_for_type(o->nametab, type);
         o->symbol = symbol_for(scope, o->name);
      }
   }
   else
      o->symbol = iterate_symbol_for(o->nametab, o->name);

   if (o->symbol != NULL) {
      for (int i = 0; i < o->symbol->ndecls; i++) {
         const decl_t *dd = get_decl(o->symbol, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (!(dd->mask & N_SUBPROGRAM))
            continue;

         tree_t next = dd->tree;
         if (dd->kind == T_ALIAS && !(next = get_aliased_subprogram(next)))
            continue;

         overload_add_candidate(o, next);
      }
   }

   overload_trace_candidates(o, "initial candidates");

   if (o->trace && o->nametab->top_type_set->members.count > 0) {
      printf("%s: context ", istr(o->name));
      for (unsigned n = 0; n < o->nametab->top_type_set->members.count; n++) {
         if (n > 0) printf(", ");
         printf("%s", type_pp(o->nametab->top_type_set->members.items[n].type));
      }
      printf("\n");
   }

   o->initial = o->candidates.count;
   o->error   = type_set_any(o->nametab, type_is_none);

   if (o->initial == 0 && !o->error && !o->trial) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(o->tree));

      if (o->symbol != NULL) {
         const bool hinted = diag_hints(d) > 0;
         for (int i = 0; i < o->symbol->ndecls; i++) {
            const decl_t *dd = get_decl(o->symbol, i);
            if ((dd->mask & N_SUBPROGRAM) && dd->visibility == HIDDEN)
               diag_hint(d, tree_loc(dd->tree), "subprogram %s is hidden",
                         type_pp(tree_type(dd->tree)));
         }

         if (!hinted)
            diag_hint(d, tree_loc(o->tree), "%s called here", istr(o->name));
      }

      type_t ptype = o->prefix ? get_type_or_null(o->prefix) : NULL;
      if (ptype != NULL && type_is_protected(ptype)) {
         diag_printf(d, "protected type %s has no method named %s",
                     type_pp(ptype), istr(o->name));

         scope_t *s = scope_for_type(o->nametab, ptype);
         hint_for_typo(s, d, o->name, N_SUBPROGRAM);
      }
      else {
         diag_printf(d, "no visible subprogram declaration for %s",
                     istr(o->name));
         hint_for_typo(o->nametab->top_scope, d, o->name, N_SUBPROGRAM);
      }

      diag_suppress(d, o->nametab->top_scope->suppress);
      diag_emit(d);
      o->error = true;
   }

   if (o->candidates.count > 1) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         type_t type = tree_type(o->candidates.items[i]);
         if (type_kind(type) == T_FUNC) {
            type_t result = type_result(type);
            if (type_set_contains(o->nametab, result))
               o->candidates.items[wptr++] = o->candidates.items[i];
            else if (tree_subkind(o->candidates.items[i]) == S_DIV_PP) {
               // LRM 93 section 7.3.5 allows an implicit conversion of
               // the result of dividing two physical types to any
               // integer type
               if (type_set_any(o->nametab, type_is_integer)) {
                  o->candidates.items[wptr++] = o->candidates.items[i];
                  o->implicit_conversion = true;
               }
               else
                  overload_prune_candidate(o, i);
            }
            else
               overload_prune_candidate(o, i);
         }
         else
            o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);
   }

   const tree_kind_t kind = tree_kind(o->tree);
   const bool is_fcall = kind == T_FCALL || kind == T_PROT_FCALL;

   // Remove procedures in a function call context and functions in a
   // procedure call context
   if (o->candidates.count > 1) {
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

   // Prune candidates with more required arguments than supplied
   if (o->candidates.count > 1) {
      int first_named = 0;
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t d = o->candidates.items[i];
         int nrequired = 0;
         const int nports = tree_ports(d);
         for (int i = 0; i < nports; i++) {
            tree_t port = tree_port(d, i);
            if (!tree_has_value(port))
               nrequired++;
            else if (first_named < o->nactuals) {
               // Named optional arguments should not count against the
               // required argument total
               ident_t id = tree_ident(port);
               for (int j = first_named; j < o->nactuals; j++) {
                  tree_t p = tree_param(o->tree, j);
                  if (tree_subkind(p) == P_NAMED) {
                     tree_t ref = name_to_ref(tree_name(p));
                     if (ref != NULL && tree_ident(ref) == id) {
                        nrequired++;
                        break;
                     }
                  }
                  else if (j <= first_named)
                     first_named = j + 1;
               }
            }
         }

         if (o->nactuals < nrequired)
            overload_prune_candidate(o, i);
         else
            o->candidates.items[wptr++] = d;
      }
      ATRIM(o->candidates, wptr);
   }

   // Filter based on required type predicate if available
   if (o->candidates.count > 1 && o->nametab->top_type_set->pred) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         type_t rtype = type_result(tree_type(o->candidates.items[i]));
         if ((*o->nametab->top_type_set->pred)(rtype))
            o->candidates.items[wptr++] = o->candidates.items[i];
         else
            overload_prune_candidate(o, i);
      }
      ATRIM(o->candidates, wptr);
   }

   // Determine if this expression could be interpreted as an indexed name
   if (is_fcall && o->nactuals > 0) {
      for (unsigned i = 0; i < o->candidates.count; i++) {
         if (!can_call_no_args(o->candidates.items[i]))
            continue;

         type_t rtype = get_result_type(o->nametab, o->candidates.items[i]);
         if (!type_is_array(rtype))
            continue;

         o->could_be_index = true;
         break;
      }

      if (o->could_be_index) {
         for (int i = 0; i < o->nactuals; i++) {
            if (tree_subkind(tree_param(o->tree, i)) == P_NAMED) {
               // Indexed name cannot have named parameters
               o->could_be_index = false;
               break;
            }
         }
      }
   }

   if (o->trial && o->nametab->top_type_set->members.count == 0) {
      // Fill in the empty type set with the result types from the
      // possible overloads
      for (unsigned i = 0; i < o->candidates.count; i++) {
         type_t type = get_result_type(o->nametab, o->candidates.items[i]);
         type_set_add(o->nametab, type, o->candidates.items[i]);
      }
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

   // Allow explicitly defined operators to hide implicitly defined ones
   // in different scopes. This is required behaviour in VHDL-2008 (see
   // section 12.4) and an optional rule relaxation in earlier revisions.
   const bool prefer_explicit = standard() >= STD_08 || relaxed_rules();

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

   // Restrict the type set to contain only the result types of the
   // final candidates
   if (o->trial && o->candidates.count > 0) {
      type_set_t *ts = o->nametab->top_type_set;
      unsigned wptr = 0;
      for (unsigned i = 0; i < ts->members.count; i++) {
         bool found = false;
         for (unsigned j = 0; !found && j < o->candidates.count; j++) {
            type_t rtype = get_result_type(o->nametab, o->candidates.items[j]);
            if (type_eq(rtype, ts->members.items[i].type))
               found = true;
            else if (o->could_be_index && type_is_array(rtype)
                     && type_eq(type_elem(rtype), ts->members.items[i].type))
               found = true;
            else if (o->implicit_conversion
                     && tree_subkind(o->candidates.items[j]) == S_DIV_PP
                     && type_is_integer(ts->members.items[i].type))
               found = true;
         }

         if (found)
            ts->members.items[wptr++] = ts->members.items[i];
      }
      ATRIM(ts->members, wptr);
   }

   tree_t result = NULL;
   if (o->candidates.count > 1 && !o->error && !o->trial) {
      const loc_t *loc = tree_loc(o->tree);
      diag_t *d = diag_new(DIAG_ERROR, loc);
      diag_printf(d, "ambiguous %s %s",
                  ident_char(o->name, 0) == '"' ? "use of operator"
                  : (tree_kind(o->tree) == T_FCALL ? "call to function"
                     : "call to procedure"),
                  istr(o->name));

      int explicit = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         const loc_t *cloc = tree_loc(o->candidates.items[i]);
         if (cloc->file_ref == loc->file_ref)
            diag_hint(d, cloc, "candidate %s",
                      type_pp(tree_type(o->candidates.items[i])));
         else {
            tree_t container = tree_container(o->candidates.items[i]);
            diag_hint(d, NULL, "candidate %s from %s",
                      type_pp(tree_type(o->candidates.items[i])),
                      istr(tree_ident(container)));
         }

         if (!(tree_flags(o->candidates.items[i]) & TREE_F_PREDEFINED))
            explicit++;
      }

      if (diag_hints(d) > 0)
         diag_hint(d, tree_loc(o->tree), "use of name %s here", istr(o->name));

      if (explicit == 1 && !prefer_explicit)
         diag_hint(d, NULL, "this would be resolved in favour of the "
                   "explicitly defined operator with $bold$--relaxed$$");

      diag_emit(d);
   }
   else if (o->candidates.count == 0 && !o->error && !o->trial) {
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
                      type_pp(ts->members.items[i].type));
      }

      tb_append(tb, ']');

      diag_t *d = diag_new(DIAG_ERROR, tree_loc(o->tree));
      diag_printf(d, "no matching %s %s",
                  ident_char(o->name, 0) == '"' ? "operator" : "subprogram",
                  tb_get(tb));

      if (o->initial > 0) {
         for (unsigned i = 0; i < o->params.count; i++) {
            tree_t pv = tree_value(o->params.items[i]);
            const tree_kind_t kind = tree_kind(pv);

            if (kind != T_LITERAL && kind != T_ATTR_REF)
               continue;
            else if (type_is_universal(tree_type(pv))) {
               diag_hint(d, NULL, "no implicit conversion was performed on "
                         "the %s argument as the innermost complete context "
                         "did not determine a unique numeric type",
                         ordinal_str(i + 1));
               diag_lrm(d, STD_08, "9.3.6");
               break;
            }
         }
      }

      diag_emit(d);
   }
   else if (o->candidates.count == 1 && !o->cancelled)
      result = o->candidates.items[0];

   ACLEAR(o->candidates);
   ACLEAR(o->params);

   return result;
}

static void overload_add_argument_type(overload_t *o, type_t type, tree_t d)
{
   type_set_add(o->nametab, type, d);
}

static void overload_positional_argument(overload_t *o, int pos)
{
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
         overload_add_argument_type(o, tree_type(p), d);
         o->candidates.items[wptr++] = d;
      }
      else if (o->initial > 1)
         overload_prune_candidate(o, i);
      else
         o->candidates.items[wptr++] = d;
   }
   ATRIM(o->candidates, wptr);

   if (o->could_be_index) {
      for (unsigned i = 0; i < o->candidates.count; i++) {
         if (!can_call_no_args(o->candidates.items[i]))
            continue;

         type_t rtype = get_result_type(o->nametab, o->candidates.items[i]);
         if (!type_is_array(rtype))
            continue;

         type_t index_type = index_type_of(rtype, pos);
         if (index_type != NULL)
            overload_add_argument_type(o, index_type, o->candidates.items[i]);
      }
   }

   o->state = O_POS;
}

static type_t overload_get_name_type(tree_t port, ident_t ident, tree_t name)
{
   if (tree_ident(port) != ident)
      return NULL;

   switch (tree_kind(name)) {
   case T_RECORD_REF:
      {
         type_t ptype = tree_type(port);
         if (!type_is_record(ptype))
            return NULL;

         const int nfields = type_fields(ptype);
         ident_t fname = tree_ident(name);
         tree_t field = NULL;
         for (int i = 0; i < nfields; i++) {
            tree_t f = type_field(ptype, i);
            if (tree_ident(f) == fname) {
               field = f;
               break;
            }
         }

         if (field == NULL)
            return NULL;   // Argument does not have this field name

         return tree_type(field);
      }
   case T_ARRAY_REF:
      {
         type_t ptype = tree_type(port);
         if (!type_is_array(ptype))
            return false;

         return type_elem(ptype);
      }
   case T_ARRAY_SLICE:
      {
         type_t ptype = tree_type(port);
         if (!type_is_array(ptype))
            return false;

         return ptype;
      }
   default:
      return tree_type(port);
   }
}

static void overload_named_argument(overload_t *o, tree_t name)
{
   assert(o->state == O_IDLE);

   type_set_push(o->nametab);

   push_scope(o->nametab);

   o->nametab->top_scope->formal_kind = F_SUBPROGRAM;
   o->nametab->top_scope->overload = o;

   tree_t ref = name_to_ref(name);
   if (ref == NULL) {
      // TODO: conversion functions not yet supported for formals
      o->state = O_NAMED;
      return;
   }

   ident_t ident = tree_ident(ref);

   if (o->trace)
      printf("%s: named argument %s\n", istr(o->name), istr(ident));

   // See if any possible overload matches this argument name
   int first_match = -1;
   for (unsigned i = 0; first_match < 0 && i < o->candidates.count; i++) {
      const int nports = tree_ports(o->candidates.items[i]);
      for (int j = 0; j < nports; j++) {
         tree_t p = tree_port(o->candidates.items[i], j);

         type_t ntype = overload_get_name_type(p, ident, name);
         if (ntype == NULL)
            continue;

         for (int j = 0; j < nports; j++) {
            tree_t p = tree_port(o->candidates.items[i], j);
            make_visible_slow(o->nametab->top_scope, tree_ident(p), p);
         }

         overload_add_argument_type(o, ntype, o->candidates.items[i]);
         first_match = i;
         break;
      }
   }

   if (first_match == -1) {
      // No possible matches so make every parameter visible to improve
      // error messages later
      for (unsigned i = 0; i < o->candidates.count; i++) {
         const int nports = tree_ports(o->candidates.items[i]);

         for (int j = 0; j < nports; j++) {
            tree_t p = tree_port(o->candidates.items[i], j);
            make_visible_slow(o->nametab->top_scope, tree_ident(p), p);
         }
      }
   }
   else {
      // Prune all overloads up to first_match
      for (int i = 0; i < first_match; i++)
         overload_prune_candidate(o, i);

      unsigned wptr = 0;
      o->candidates.items[wptr++] = o->candidates.items[first_match];

      // Prune any remaining overloads which do not have this named argument
      for (unsigned i = first_match + 1; i < o->candidates.count; i++) {
         const int nports = tree_ports(o->candidates.items[i]);
         tree_t port = NULL;
         for (int j = 0; j < nports; j++) {
            tree_t p = tree_port(o->candidates.items[i], j);

            type_t ntype = overload_get_name_type(p, ident, name);
            if (ntype == NULL)
               continue;

            overload_add_argument_type(o, ntype, o->candidates.items[i]);

            port = p;
            break;
         }

         if (port == NULL)
            overload_prune_candidate(o, i);
         else {
            o->candidates.items[wptr++] = o->candidates.items[i];

            for (int j = 0; j < nports; j++) {
               tree_t p = tree_port(o->candidates.items[i], j);
               make_visible_slow(o->nametab->top_scope, tree_ident(p), p);
            }
         }
      }
      ATRIM(o->candidates, wptr);
   }

   o->state = O_NAMED;
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
   assert(tree_kind(p) == T_PARAM);
   assert(o->state != O_IDLE);

   tree_t value = tree_value(p);
   type_t type = tree_type(value);

   if (o->trace) {
      printf("%s: next argument %s %s\n", istr(o->name),
             tree_kind_str(tree_kind(value)), type_pp(type));
   }

   const bool is_partial =
      tree_subkind(p) == P_NAMED
      && tree_kind(tree_name(p)) != T_REF;

   if (o->initial > 1 && !is_partial) {
      unsigned wptr = 0;
      int first_match = -1;
      bool found_port = false;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t port = overload_find_port(o->candidates.items[i], p);
         if (port != NULL && type != NULL) {
            type_t ptype = tree_type(port);
            found_port = true;

            if (type_eq(ptype, type)) {
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

   // If the parameter is not of a discrete type then this definitely
   // cannot be an indexed name
   if (o->could_be_index && !type_is_discrete(type))
      o->could_be_index = false;

   if (type != NULL && type_is_none(type))
      o->error = true;  // Suppress further errors

   APUSH(o->params, p);

   type_set_pop(o->nametab);

   o->state = O_IDLE;
}

static void overload_cancel_argument(overload_t *o, tree_t p)
{
   assert(o->state != O_IDLE);

   if (o->trace)
      printf("%s: cancel argument\n", istr(o->name));

   // Use the restricted type set to prune candidates which could never
   // match this argument
   type_set_t *ts = o->nametab->top_type_set;
   if (ts->members.count > 0 && o->candidates.count > 1) {
      if (o->trace) {
         printf("%s: restrict to: ", istr(o->name));
         for (unsigned n = 0; n < ts->members.count; n++) {
            if (n > 0) printf(", ");
            printf("%s", type_pp(ts->members.items[n].type));
         }
         printf("\n");
      }

      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t port = overload_find_port(o->candidates.items[i], p);
         if (port != NULL && type_set_contains(o->nametab, tree_type(port)))
            o->candidates.items[wptr++] = o->candidates.items[i];
         else
            overload_prune_candidate(o, i);
      }
      ATRIM(o->candidates, wptr);
   }

   type_set_pop(o->nametab);
   o->state = O_IDLE;
}

void map_generic_box(nametab_t *tab, tree_t inst, tree_t g, unsigned pos)
{
   // Find the actual for the <> "box" default generic subprogram

   assert(tree_kind(g) == T_GENERIC_DECL);

   tree_t box = tree_value(g);
   assert(tree_kind(box) == T_BOX);

   type_t type = tree_type(g);
   ident_t func = tree_has_ident(box) ? tree_ident(box) : type_ident(type);

   tree_t ref = tree_new(T_REF);
   tree_set_loc(ref, tree_loc(inst));
   tree_set_ident(ref, func);

   tree_t decl = NULL;
   const symbol_t *sym = iterate_symbol_for(tab, func);
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN || !(dd->mask & N_SUBPROGRAM))
            continue;
         else if (dd->tree == g)
            continue;

         tree_t sub = dd->tree;
         if (dd->kind == T_ALIAS && !(sub = get_aliased_subprogram(sub)))
            continue;

         type_t signature = tree_type(sub);
         if (!type_eq_map(type, signature, tab->top_scope->gmap))
            continue;

         decl = sub;
         break;
      }
   }

   if (decl != NULL) {
      tree_set_ref(ref, decl);
      tree_set_type(ref, tree_type(decl));

      map_generic_subprogram(tab, g, decl);
   }
   else if (!tab->top_scope->suppress) {
      const char *signature = strchr(type_pp(type), '[');
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(inst));
      diag_printf(d, "no visible subprogram %s matches signature %s",
                  istr(func), signature);
      diag_hint(d, tree_loc(box), "while resolving interface subprogram "
                "default for %s", istr(type_ident(type)));

      if (tree_flags(g) & TREE_F_PREDEFINED) {
         type_t actual = NULL;
         if (tab->top_scope->gmap != NULL)
            actual = hash_get(tab->top_scope->gmap, type_param(type, 0));

         if (actual != NULL && !type_is_none(actual)) {
            diag_hint(d, NULL, "add a use clause that makes the operators for "
                      "%s visible", istr(type_ident(actual)));

            if (standard() == STD_08)
               diag_hint(d, NULL, "the 2008 LRM is unclear on the visibility "
                         "requirements for the implicit operators associated "
                         "with a generic type, and behaviour varies between "
                         "implementations; the interpretation here is "
                         "consistent with the 2019 LRM");

            diag_lrm(d, STD_08, "6.5.3");
         }

         suppress_errors(tab);   // Avoid error for both "=" and "/="
      }

      diag_emit(d);
   }

   tree_t map = tree_new(T_PARAM);
   tree_set_loc(ref, tree_loc(inst));
   tree_set_subkind(map, P_POS);
   tree_set_pos(map, pos);
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
      scope_t *owner = NULL;
      const symbol_t *sym = symbol_for(tab->top_scope, tree_ident(decl));
      if (sym != NULL) {
         for (int i = 0; i < sym->ndecls; i++) {
            const decl_t *dd = get_decl(sym, i);
            if (dd->tree == decl) {
               owner = dd->origin;
               break;
            }
         }
      }

      if (owner != NULL && scope_find_enclosing(owner, S_SUBPROGRAM) != sub)
         error_at(tree_loc(ref), "invalid reference to %s inside pure "
                  "function %s", istr(tree_ident(decl)), istr(tree_ident(sub)));
   }
}

////////////////////////////////////////////////////////////////////////////////
// Type solver

static bool is_unambiguous(tree_t t)
{
   if (tree_has_type(t))
      return true;

   const tree_kind_t kind = tree_kind(t);
   return kind == T_QUALIFIED
      || kind == T_ARRAY_REF
      || kind == T_ARRAY_SLICE
      || kind == T_TYPE_CONV
      || kind == T_RECORD_REF
      || kind == T_ALL
      || (kind == T_REF && tree_has_ref(t));
}

static bool solve_one_param(nametab_t *tab, tree_t p, overload_t *o, bool trial)
{
   switch (tree_subkind(p)) {
   case P_POS:
      overload_positional_argument(o, tree_pos(p));
      break;
   case P_NAMED:
      {
         tree_t name = tree_name(p);
         overload_named_argument(o, name);
         solve_types(tab, name, NULL);
         pop_scope(o->nametab);
      }
      break;
   }

   tree_t value = tree_value(p);
   if (!trial || is_unambiguous(value)) {
      _solve_types(o->nametab, value);
      overload_next_argument(o, p);
      return true;
   }
   else if (!try_solve_type(o->nametab, value)) {
      overload_cancel_argument(o, p);
      return false;
   }
   else {
      overload_next_argument(o, p);
      return true;
   }
}

static void solve_subprogram_params(nametab_t *tab, tree_t call, overload_t *o)
{
   const int nparams = tree_params(call);

   LOCAL_BIT_MASK pmask;
   mask_init(&pmask, nparams);

   // Make an initial pass over the parameters and solve the "easy" ones

   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(call, i);
      tree_t value = tree_value(p);
      if (is_unambiguous(value)) {
         solve_one_param(tab, p, o, false);
         mask_set(&pmask, i);
      }
   }

   // Attempt to solve remaining parameters but suppress errors

   for (int i = 0; i < nparams; i++) {
      if (mask_test(&pmask, i))
         continue;

      tree_t p = tree_param(call, i);
      tree_t value = tree_value(p);

      const bool trial = o->trial
         || (o->candidates.count > 1 && mask_popcount(&pmask) + 1 < nparams);

      if (o->error && o->initial == 0) {
         // Avoid cascading errors from an undefined subprogram
         tree_set_type(value, type_new(T_NONE));
         mask_set(&pmask, i);
      }
      else if (solve_one_param(tab, p, o, trial))
         mask_set(&pmask, i);
   }

   if (o->trial && mask_popcount(&pmask) < nparams) {
      o->cancelled = true;
      return;   // Cannot identify the overload yet
   }

   // Catch-all pass for remaining parameters

   for (int i = 0; i < nparams; i++) {
      if (mask_test(&pmask, i))
         continue;

      tree_t p = tree_param(call, i);
      if (o->error)
         tree_set_type(tree_value(p), type_new(T_NONE));
      else
         solve_one_param(tab, p, o, false);
   }
}

static type_t resolve_fcall_or_index(nametab_t *tab, tree_t fcall, tree_t decl)
{
   // The expression A(X) can be parsed as a call to subprogram A with
   // no arguments, returning an array that is indexed by X, or a call
   // to subprogram A with argument X. We prefer the latter and then
   // fix-up here based on the context.

   type_t rtype = get_result_type(tab, decl);

   if (!type_is_array(rtype))
      return rtype;

   if (!can_call_no_args(decl))
      return rtype;

   if (tree_params(fcall) != dimension_of(rtype))
      return rtype;

   if (tab->top_type_set->members.count == 0)
      return rtype;    // Context cannot disambiguate

   type_t etype = type_elem(rtype);

   if (!type_set_contains(tab, etype))
      return rtype;

   const tree_kind_t kind = tree_kind(fcall);

   tree_t new = tree_new(kind);
   tree_set_ref(new, decl);
   tree_set_ident(new, tree_ident(fcall));
   tree_set_loc(new, tree_loc(fcall));
   tree_set_type(new, rtype);

   if (kind == T_PROT_FCALL && tree_has_name(fcall))
      tree_set_name(new, tree_name(fcall));

   tree_change_kind(fcall, T_ARRAY_REF);
   tree_set_value(fcall, new);

   return etype;
}

static void nest_protected_call(tree_t call, tree_t pref)
{
   tree_t prefix = tree_name(call);
   tree_t value = tree_value(pref);

   tree_t nest = tree_new(T_PROT_REF);
   tree_set_value(nest, prefix);
   tree_set_ref(nest, tree_ref(value));
   tree_set_ident(nest, tree_ident(value));
   tree_set_type(nest, tree_type(value));

   tree_set_name(call, nest);
   tree_set_ref(call, tree_ref(pref));
}

static type_t resolve_fcall(nametab_t *tab, tree_t fcall, tree_t decl,
                            bool could_be_index)
{
   type_t ftype = tree_type(decl);
   const type_kind_t typek = type_kind(ftype);
   if (typek == T_PROC) {
      error_at(tree_loc(fcall), "procedure %s not allowed in an expression",
               istr(tree_ident(fcall)));
      return type_new(T_NONE);
   }

   assert(typek == T_FUNC);

   if (tree_kind(decl) == T_PROT_REF) {
      // Calling an alias of a protected type method
      if (tree_kind(fcall) == T_PROT_FCALL)
         nest_protected_call(fcall, decl);
      else {
         tree_change_kind(fcall, T_PROT_FCALL);
         tree_set_name(fcall, tree_value(decl));
      }

      decl = tree_ref(decl);
   }

   tree_set_ref(fcall, decl);

   const tree_flags_t flags = tree_flags(decl);
   if ((flags & TREE_F_PROTECTED) && tree_kind(fcall) != T_PROT_FCALL)
      tree_change_kind(fcall, T_PROT_FCALL);

   if ((flags & TREE_F_KNOWS_SUBTYPE) && !tab->top_type_set->known_subtype)
      error_at(tree_loc(fcall), "function %s with return identifier %s "
               "cannot be called in this context as the result subtype "
               "is not known", istr(tree_ident(decl)),
               istr(type_ident(type_result(ftype))));

   if (could_be_index)
      return resolve_fcall_or_index(tab, fcall, decl);
   else if (tree_subkind(decl) == S_DIV_PP) {
      // LRM 93 section 7.3.5 an implicit conversion is applied if the
      // operand is an expression consisting of the division of a value
      // of a physical type by a value of the same type and the context
      // determines a unique numeric type

      type_set_restrict(tab, type_is_integer);

      type_t type;
      if (type_set_uniq(tab, &type))
         return type;
   }

   return get_result_type(tab, decl);
}

static type_t try_solve_fcall(nametab_t *tab, tree_t fcall)
{
   if (tree_has_type(fcall))
      return tree_type(fcall);

   const tree_kind_t kind = tree_kind(fcall);

   overload_t o = {
      .tree     = fcall,
      .state    = O_IDLE,
      .nametab  = tab,
      .trace    = false,
      .trial    = true,
      .prefix   = kind == T_PROT_FCALL ? tree_name(fcall) : NULL,
      .nactuals = tree_params(fcall),
      .name     = tree_ident(fcall),
   };
   begin_overload_resolution(&o);

   solve_subprogram_params(tab, fcall, &o);

   tree_t decl = finish_overload_resolution(&o);
   if (decl == NULL)
      return NULL;

   // This expression may stil be ambiguous between a function call and
   // an indexed name if the context has more than one type
   if (o.could_be_index && tab->top_type_set->members.count > 1)
      return NULL;

   type_t type = resolve_fcall(tab, fcall, decl, o.could_be_index);
   tree_set_type(fcall, type);
   return type;
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
      .prefix   = kind == T_PROT_FCALL ? tree_name(fcall) : NULL,
      .nactuals = tree_params(fcall),
      .name     = tree_ident(fcall),
   };
   begin_overload_resolution(&o);

   solve_subprogram_params(tab, fcall, &o);

   type_t type;
   tree_t decl = finish_overload_resolution(&o);
   if (decl == NULL)
      type = type_new(T_NONE);
   else
      type = resolve_fcall(tab, fcall, decl, o.could_be_index);

   tree_set_type(fcall, type);
   return type;
}

static type_t solve_pcall(nametab_t *tab, tree_t pcall)
{
   const tree_kind_t kind = tree_kind(pcall);

   overload_t o = {
      .name     = tree_ident2(pcall),
      .tree     = pcall,
      .state    = O_IDLE,
      .nametab  = tab,
      .trace    = false,
      .prefix   = kind == T_PROT_PCALL ? tree_name(pcall) : NULL,
      .nactuals = tree_params(pcall)
   };
   begin_overload_resolution(&o);

   solve_subprogram_params(tab, pcall, &o);

   tree_t decl = finish_overload_resolution(&o);
   if (decl != NULL && tree_kind(decl) == T_PROT_REF) {
      // Calling an alias of a protected type method
      if (kind == T_PROT_PCALL)
         nest_protected_call(pcall, decl);
      else {
         tree_change_kind(pcall, T_PROT_PCALL);
         tree_set_ref(pcall, tree_ref(decl));
         tree_set_name(pcall, tree_value(decl));
      }
   }
   else if (decl != NULL) {
      tree_set_ref(pcall, decl);
      if ((tree_flags(decl) & TREE_F_PROTECTED) && kind == T_PCALL)
         tree_change_kind(pcall, T_PROT_PCALL);
   }

   return NULL;  // Procedure call has no type
}

static type_t try_solve_string(nametab_t *tab, tree_t str)
{
   if (tree_has_type(str))
      return tree_type(str);
   else if (type_set_any(tab, type_is_none)) {
      type_t type = type_new(T_NONE);   // Suppress cascading errors
      tree_set_type(str, type);
      return type;
   }

   // The type must be determinable soley from the context excluding the
   // literal itself but using the fact that the type must be a one
   // dimensional array of a character type

   type_set_restrict(tab, type_is_character_array);

   type_t type = NULL;
   if (!type_set_uniq(tab, &type))
      return NULL;

   type_t elem = type_elem(type);
   const int nchars = tree_chars(str);
   for (int i = 0; i < nchars; i++)
      solve_types(tab, tree_char(str, i), elem);

   type_t sub = subtype_for_string(str, type);
   tree_set_type(str, sub);
   return sub;
}

static type_t solve_string(nametab_t *tab, tree_t str)
{
   type_t type = try_solve_string(tab, str);
   if (type != NULL)
      return type;

   diag_t *d = diag_new(DIAG_ERROR, tree_loc(str));
   diag_printf(d, "type of string literal cannot be determined "
               "from the surrounding context");
   type_set_describe(tab, d, tree_loc(str), type_is_character_array,
                     "a one dimensional array of character type");
   diag_emit(d);

   tree_set_type(str, (type = type_new(T_NONE)));
   return type;
}

static type_t try_solve_literal(nametab_t *tab, tree_t lit)
{
   if (tree_has_type(lit))
      return tree_type(lit);

   switch (tree_subkind(lit)) {
   case L_NULL:
      {
         type_set_restrict(tab, type_is_access);

         type_t type;
         if (!type_set_uniq(tab, &type))
            return NULL;

         tree_set_type(lit, type);
         return type;
      }

   case L_PHYSICAL:
      {
         ident_t id = tree_ident(lit);
         type_t type;
         tree_t decl = resolve_name(tab, tree_loc(lit), id);
         if (decl == NULL)
            return NULL;
         else if (class_of(decl) != C_UNITS) {
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

   case L_INT:
      {
         type_set_restrict(tab, type_is_integer);

         type_t type;
         if (!type_set_uniq(tab, &type))
            return NULL;

         tree_set_type(lit, type);
         return type;
      }

   case L_REAL:
      {
         type_set_restrict(tab, type_is_real);

         type_t type;
         if (!type_set_uniq(tab, &type))
            return NULL;

         tree_set_type(lit, type);
         return type;
      }

   default:
      fatal_at(tree_loc(lit), "cannot solve literal");
   }
}

static type_t solve_literal(nametab_t *tab, tree_t lit)
{
   type_t type = try_solve_literal(tab, lit);
   if (type != NULL)
      return type;

   switch (tree_subkind(lit)) {
   case L_INT:
      tree_set_type(lit, (type = std_type(NULL, STD_UNIVERSAL_INTEGER)));
      return type;
   case L_REAL:
      tree_set_type(lit, (type = std_type(NULL, STD_UNIVERSAL_REAL)));
      return type;
   case L_NULL:
      error_at(tree_loc(lit), "type of null expression cannot be determined "
               "from the surrounding context");
      // Fall-through
   default:
      tree_set_type(lit, (type = type_new(T_NONE)));
      return type;
   }
}

static type_t try_solve_ref(nametab_t *tab, tree_t ref)
{
   if (tree_has_type(ref))
      return tree_type(ref);

   tree_t decl = try_resolve_name(tab, tree_ident(ref));
   if (decl == NULL)
      return NULL;

   type_t type = get_type_or_null(decl);
   if (type == NULL || type_is_subprogram(type))
      return NULL;

   tree_set_ref(ref, decl);
   tree_set_type(ref, type);
   return type;
}

static type_t solve_ref(nametab_t *tab, tree_t ref)
{
   if (tree_has_type(ref))
      return tree_type(ref);

   tree_t decl = resolve_ref(tab, ref);
   type_t type = NULL;

   if (decl == NULL) {
      tree_set_type(ref, (type = type_new(T_NONE)));
      return type;
   }

   const class_t class = class_of(decl);
   if (!class_has_type(class)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(ref));
      diag_printf(d, "invalid use of %s %s", class_str(class_of(decl)),
                  istr(tree_ident(ref)));

      // Try to provide a more helpful error if an object with the same
      // name is hidden
      const symbol_t *sym = iterate_symbol_for(tab, tree_ident(ref));
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN && get_type_or_null(dd->tree) != NULL)
            diag_hint(d, tree_loc(dd->tree), "declaration of %s %s is hidden",
                      class_str(class_of(dd->tree)), istr(sym->name));
      }

      diag_hint(d, tree_loc(decl), "name %s refers to this %s",
                istr(sym->name), class_str(class));
      diag_suppress(d, tab->top_scope->suppress);
      diag_emit(d);

      tree_set_ref(ref, NULL);
      tree_set_type(ref, (type = type_new(T_NONE)));
      return type;
   }

   if (tree_kind(decl) == T_ALIAS) {
      // An alias declaration for an enumeration literal may contain a
      // signature so make sure it doesn't look like a subprogram
      tree_t aliased = tree_value(decl);
      if (!tree_has_type(decl) || class_of(aliased) == C_LITERAL)
         type = tree_type(aliased);
      else
         type = tree_type(decl);
   }
   else
      type = tree_type(decl);

   if (type_is_subprogram(type)) {
      type_t constraint;
      const bool want_ref =
         type_set_uniq(tab, &constraint) && type_is_subprogram(constraint);

      if (can_call_no_args(decl) && !want_ref) {
         tree_change_kind(ref, T_FCALL);
         return solve_fcall(tab, ref);
      }
   }
   else
      check_pure_ref(tab, ref, decl);

   tree_set_ref(ref, decl);
   tree_set_type(ref, type);
   return type;
}

static type_t solve_prot_ref(nametab_t *tab, tree_t pref)
{
   type_t constraint;
   if (type_set_uniq(tab, &constraint) && type_is_subprogram(constraint)) {
      // Reference to protected type subprogram with signature
      tree_t decl = resolve_method_name(tab, pref, constraint);
      type_t type = decl ? tree_type(decl) : type_new(T_NONE);
      tree_set_type(pref, type);
      tree_set_ref(pref, decl);
      return type;
   }

   error_at(tree_loc(pref), "invalid use of name %s", istr(tree_ident(pref)));

   type_t type = type_new(T_NONE);
   tree_set_type(pref, type);
   return type;
}

static type_t solve_field_subtype(type_t rtype, tree_t field)
{
   type_t ftype = tree_type(field);

   if (type_is_unconstrained(ftype)) {
      // Get the subtype from the record element constraint
      tree_t cons = type_constraint_for_field(rtype, field);
      if (cons != NULL) {
         type_t sub = tree_type(cons);
         assert(type_kind(sub) == T_SUBTYPE);
         return sub;
      }
   }

   return ftype;
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
   else if (!type_is_record(value_type)) {
      error_at(tree_loc(rref), "type %s is not a record", type_pp(value_type));
      tree_set_type(rref, (value_type = type_new(T_NONE)));
      return value_type;
   }

   tree_t field = find_record_field(rref);

   type_t type;
   if (field == NULL) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(rref));
      diag_printf(d, "record type %s has no field named %s",
                  type_pp(value_type), istr(tree_ident(rref)));

      ident_t best = NULL;
      int bestd = INT_MAX;

      LOCAL_TEXT_BUF tb = tb_new();
      int nfields = type_fields(value_type), i;
      tb_printf(tb, "type %s has fields ", type_pp(value_type));
      for (i = 0; i < nfields; i++) {
         ident_t id = tree_ident(type_field(value_type, i));
         const int d = ident_distance(id, tree_ident(rref));
         if (d < bestd) {
            best = id;
            bestd = d;
         }
         if (i > 0 && i == nfields - 1) tb_cat(tb, ", and ");
         else if (i > 0) tb_cat(tb, ", ");
         tb_istr(tb, id);
      }
      diag_hint(d, NULL, "%s", tb_get(tb));

      if (bestd <= 3)
         diag_hint(d, tree_loc(rref), "did you mean %s?", istr(best));

      diag_emit(d);
      type = type_new(T_NONE);
   }
   else
      type = solve_field_subtype(value_type, field);

   tree_set_ref(rref, field);
   tree_set_type(rref, type);
   return type;
}

static type_t solve_array_ref(nametab_t *tab, tree_t ref)
{
   if (tree_has_type(ref))
      return tree_type(ref);

   type_t base_type = solve_types(tab, tree_value(ref), NULL);

   const int nparams = tree_params(ref);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(ref, i);
      assert(tree_subkind(p) == P_POS);

      type_t index_type = index_type_of(base_type, i);
      solve_types(tab, tree_value(p), index_type);
   }

   type_t elem_type;
   if (type_is_array(base_type))
      elem_type = type_elem(base_type);
   else if (type_is_none(base_type))
      elem_type = base_type;
   else {
      error_at(tree_loc(ref), "cannot index non-array type %s",
               type_pp(base_type));
      elem_type = type_new(T_NONE);
   }

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
   type_add_constraint(slice_type, constraint);
   type_set_base(slice_type, base_type);

   tree_set_type(slice, slice_type);
   return slice_type;
}

static type_t try_solve_attr_ref(nametab_t *tab, tree_t aref)
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
      case ATTR_PRED:
      case ATTR_SUCC:
         type_set_add(tab, prefix_type, NULL);
         break;
      case ATTR_VALUE:
         type_set_add(tab, std_type(NULL, STD_STRING), NULL);
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
      if (prefix_type == NULL) {
         error_at(tree_loc(aref), "prefix does not have a range");
         type = type_new(T_NONE);
      }
      else if (type_is_array(prefix_type)) {
         int64_t dim = 1;
         if (nparams > 0) {
            tree_t pdim = tree_value(tree_param(aref, 0));

            // The type can be wrong if this expression is not a literal
            // but there does not seem to be any easy way to handle this
            (void)folded_int(pdim, &dim);
         }

         if ((type = index_type_of(prefix_type, dim - 1)) == NULL) {
            error_at(tree_loc(aref), "dimension index %"PRIi64" out of range "
                     "for type %s", dim, type_pp(prefix_type));
            type = type_new(T_NONE);
         }
      }
      else
         type = prefix_type;
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
   case ATTR_CONVERSE:
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
      // LRM 08 section 9.3.6: implicit conversion of attribute to a
      // unique numeric type from the context
      type_set_restrict(tab, type_is_integer);
      if (!type_set_uniq(tab, &type))
         return NULL;
      break;

   case ATTR_BASE:
      if (prefix_type != NULL) {
         if (type_kind(prefix_type) == T_SUBTYPE)
            type = type_base(prefix_type);
         else
            type = prefix_type;
      }
      break;

   case ATTR_SUBTYPE:
   case ATTR_ELEMENT:
   case ATTR_DESIGNATED_SUBTYPE:
   case ATTR_INDEX:
      // These should have been expanded by the parser so can only be
      // erroneous
      type = prefix_type;
      break;

   case ATTR_REFLECT:
      {
         bool prefix_is_type = false;
         switch (tree_kind(prefix)) {
         case T_REF:
            if (tree_has_ref(prefix))
               prefix_is_type = (aliased_type_decl(tree_ref(prefix)) != NULL);
            break;
         case T_ATTR_REF:
            prefix_is_type = is_type_attribute(tree_subkind(prefix));
            break;
         default:
            break;
         }

         if (prefix_is_type)
            type = reflection_type(REFLECT_SUBTYPE_MIRROR);
         else
            type = reflection_type(REFLECT_VALUE_MIRROR);
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

            scope_t *s = tab->top_scope;
            if (is_container(decl))
               s = private_scope_for(tab, decl);

            const symbol_t *sym = symbol_for(s, attr);
            if (sym != NULL) {
               for (int i = 0; i < sym->ndecls; i++) {
                  const decl_t *dd = get_decl(sym, i);
                  if (dd->visibility != ATTRIBUTE)
                     continue;
                  else if (tree_class(dd->tree) != class)
                     continue;

                  const spec_kind_t kind = tree_subkind(dd->tree);
                  if ((kind == SPEC_EXACT && tree_ident2(dd->tree) == id)
                      || kind == SPEC_ALL) {
                     a = dd->tree;
                     break;
                  }
                  else if (kind == SPEC_OTHERS)
                     a = dd->tree;
               }
            }

            if (a != NULL) {
               tree_set_value(aref, tree_value(a));
               type = tree_type(a);
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

static type_t solve_attr_ref(nametab_t *tab, tree_t aref)
{
   type_t type = try_solve_attr_ref(tab, aref);
   if (type != NULL)
      return type;

   switch (tree_subkind(aref)) {
   case ATTR_POS:
      tree_set_type(aref, (type = std_type(NULL, STD_UNIVERSAL_INTEGER)));
      return type;

   default:
      fatal_trace("attribute %s cannot be ambiguous", istr(tree_ident(aref)));
   }
}

static type_t solve_record_aggregate(nametab_t *tab, tree_t agg, type_t type)
{
   const int nfields = type_fields(type);
   const int nassocs = tree_assocs(agg);

   // Mask used for finding the types of an "others" association
   bit_mask_t fmask;
   mask_init(&fmask, nfields);

   for (int i = 0; i < nassocs; i++) {
      type_set_push(tab);
      tree_t a = tree_assoc(agg, i);

      switch (tree_subkind(a)) {
      case A_POS:
         {
            int pos = tree_pos(a);
            if (pos < nfields) {
               tree_t f = type_field(type, pos);
               type_set_add(tab, solve_field_subtype(type, f), f);
            }
            if (pos < nfields) mask_set(&fmask, pos);
         }
         break;

      case A_NAMED:
         {
            push_scope_for_fields(tab, type);
            tree_t name = tree_name(a);
            solve_types(tab, name, NULL);
            pop_scope(tab);
            if (tree_has_ref(name)) {
               tree_t field = tree_ref(name);
               type_set_add(tab, solve_field_subtype(type, field), field);
               if (tree_kind(field) == T_FIELD_DECL) {
                  const int pos = tree_pos(field);
                  if (pos < nfields) mask_set(&fmask, pos);
               }
            }
            else {
               // Error in field name
               type_set_add(tab, type_new(T_NONE), NULL);
            }
         }
         break;

      case A_OTHERS:
         // Add the types of all the fields that haven't already be
         // specified to the type set
         for (int i = 0; i < MIN(nfields, 64); i++) {
            if (!mask_test(&fmask, i)) {
               tree_t f = type_field(type, i);
               type_set_add(tab, tree_type(f), f);
            }
         }
         break;

      case A_RANGE:
         // This is illegal and will generate an error during
         // semantic checking
         push_scope_for_fields(tab, type);
         solve_types(tab, tree_range(a, 0), NULL);
         pop_scope(tab);
         break;
      }

      _solve_types(tab, a);
      type_set_pop(tab);
   }

   mask_free(&fmask);
   return type;
}

static type_t solve_array_aggregate(nametab_t *tab, tree_t agg, type_t type)
{
   // All elements must be of the composite base type if this is a
   // one-dimensional array otherwise construct an array type with
   // n-1 dimensions.

   type_set_push(tab);

   type_t t0, t1 = NULL;
   const int ndims = dimension_of(type);
   if (ndims == 1) {
      type_t elem = type_elem(type);
      type_set_add(tab, (t0 = elem), NULL);

      if (standard() >= STD_08 && !type_is_composite(elem))
         type_set_add(tab, (t1 = type_base_recur(type)), NULL);
   }
   else
      type_set_add(tab, (t0 = array_aggregate_type(type, 1)), NULL);

   type_t index_type = index_type_of(type, 0);

   bool have_named = false, have_others = false;
   const int nassocs = tree_assocs(agg);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(agg, i);

      const assoc_kind_t kind = tree_subkind(a);
      switch (kind) {
      case A_POS:
      case A_CONCAT:
      case A_SLICE:
         break;
      case A_OTHERS:
         have_others = true;
         break;
      case A_NAMED:
         {
            tree_t name = tree_name(a);
            type_t ntype = solve_types(tab, name, index_type);

            if (tree_kind(name) == T_REF && tree_has_ref(name)) {
               tree_t type_decl = aliased_type_decl(tree_ref(name));
               if (type_decl != NULL) {
                  // This should have been parsed as a range association
                  tree_t tmp = tree_new(T_ATTR_REF);
                  tree_set_name(tmp, name);
                  tree_set_ident(tmp, ident_new("RANGE"));
                  tree_set_loc(tmp, tree_loc(name));
                  tree_set_subkind(tmp, ATTR_RANGE);
                  tree_set_type(tmp, ntype);

                  tree_t r = tree_new(T_RANGE);
                  tree_set_subkind(r, RANGE_EXPR);
                  tree_set_value(r, tmp);
                  tree_set_loc(r, tree_loc(name));
                  tree_set_type(r, ntype);

                  tree_set_subkind(a, A_RANGE);
                  tree_add_range(a, r);
               }
            }

            have_named = true;
         }
         break;
      case A_RANGE:
         solve_types(tab, tree_range(a, 0), index_type);
         have_named = true;
         break;
      }

      type_t etype = _solve_types(tab, tree_value(a));

      // Hack to avoid pushing/popping type set on each iteration
      ATRIM(tab->top_type_set->members, 0);
      type_set_add(tab, t0, NULL);
      type_set_add(tab, t1, NULL);

      if (t1 != NULL && type_eq(etype, t1)) {
         // VHDL-2008 slice in aggregate
         switch (kind) {
         case A_RANGE: tree_set_subkind(a, A_SLICE); break;
         case A_POS: tree_set_subkind(a, A_CONCAT); break;
         default: break;
         }
      }
   }

   type_set_pop(tab);

   bool bounds_from_context = true;
   if (type_is_unconstrained(type))
      bounds_from_context = false;
   else if (have_named && !have_others && is_anonymous_subtype(type))
      bounds_from_context = false;

   if (bounds_from_context)
      return type;

   type_t sub = calculate_aggregate_subtype(agg);
   if (sub == NULL) {
      sub = type_new(T_SUBTYPE);
      type_set_base(sub, type_base_recur(type));
      type_set_elem(sub, type_elem(type));

      tree_t cons = tree_new(T_CONSTRAINT);
      tree_set_subkind(cons, C_OPEN);

      type_add_constraint(sub, cons);
   }

   tree_set_type(agg, sub);
   return sub;
}

static type_t try_solve_aggregate(nametab_t *tab, tree_t agg)
{
   if (tree_has_type(agg))
      return tree_type(agg);

   // The type of an aggregate must be determinable solely from the
   // context in which the aggregate appears

   type_t type;
   if (type_set_any(tab, type_is_none))
      type = type_new(T_NONE);
   else {
      type_set_restrict(tab, type_is_composite);

      if (!type_set_uniq(tab, &type))
         return NULL;
   }

   tree_set_type(agg, type);

   if (type_is_record(type))
      return solve_record_aggregate(tab, agg, type);
   else
      return solve_array_aggregate(tab, agg, type);
}

static type_t solve_aggregate(nametab_t *tab, tree_t agg)
{
   type_t type = try_solve_aggregate(tab, agg);
   if (type != NULL)
      return type;

   diag_t *d = diag_new(DIAG_ERROR, tree_loc(agg));
   diag_printf(d, "type of aggregate cannot be determined "
               "from the surrounding context");
   type_set_describe(tab, d, tree_loc(agg), type_is_composite,
                     "a composite type");

   diag_emit(d);

   tree_set_type(agg, (type = type_new(T_NONE)));
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

   type_set_restrict(tab, type_is_access);

   type_t type;
   if (!type_set_uniq(tab, &type)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(new));
      diag_printf(d, "cannot determine type of allocator expression "
                  "from the surrounding context");
      type_set_describe(tab, d, NULL, type_is_access, "an access type");
      diag_emit(d);

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

   type_t type = solve_types(tab, prefix, NULL);
   if (type_is_access(type)) {
      type = type_designated(type);
      if (type_is_incomplete(type)
          && (type = resolve_type(tab, type)) == NULL) {
         error_at(tree_loc(prefix), "object with incomplete type %s "
                  "cannot be dereferenced", type_pp(type));
         type = type_new(T_NONE);
      }
   }

   tree_set_type(all, type);
   return type;
}

static type_t try_solve_open(nametab_t *tab, tree_t open)
{
   if (tree_has_type(open))
      return tree_type(open);

   type_t type;
   if (!type_set_uniq(tab, &type))
      return NULL;

   tree_set_type(open, type);
   return type;
}

static type_t solve_open(nametab_t *tab, tree_t open)
{
   type_t type = try_solve_open(tab, open);
   if (type != NULL)
      return NULL;

   error_at(tree_loc(open), "cannot determine type of OPEN expression");
   tree_set_type(open, (type = type_new(T_NONE)));
   return type;
}

static type_t solve_external_name(nametab_t *tab, tree_t name)
{
   if (tree_has_type(name))
      return tree_type(name);

   assert(error_count() > 0);
   type_t none = type_new(T_NONE);
   tree_set_type(name, none);
   return none;
}

static type_t solve_cond_value(nametab_t *tab, tree_t value)
{
   type_t type0 = NULL;

   const int nconds = tree_conds(value);
   for (int i = 0; i < nconds; i++) {
      type_t type = _solve_types(tab, tree_cond(value, i));
      if (type0 == NULL)
         type0 = type;
   }

   type_t type;
   if (!type_set_uniq(tab, &type))
      type = type0;

   tree_set_type(value, type);
   return type;
}

static type_t solve_cond_expr(nametab_t *tab, tree_t value)
{
   // The condition should have already been checked by solve_condition
   if (tree_has_result(value))
      return _solve_types(tab, tree_result(value));
   else
      return NULL;
}

static type_t solve_view_element(nametab_t *tab, tree_t elem)
{
   tree_t f = resolve_name(tab, tree_loc(elem), tree_ident(elem));
   if (f == NULL) {
      type_t none = type_new(T_NONE);
      tree_set_type(elem, none);
      return none;
   }

   assert(tree_kind(f) == T_FIELD_DECL);
   tree_set_ref(elem, f);

   if (tree_has_type(elem))
      return tree_type(elem);   // Subtype from mode view
   else {
      type_t type = tree_type(f);
      tree_set_type(elem, type);
      return type;
   }
}

static type_t solve_inertial(nametab_t *tab, tree_t expr)
{
   type_t type = _solve_types(tab, tree_value(expr));
   tree_set_type(expr, type);
   return type;
}

static type_t solve_range(nametab_t *tab, tree_t r)
{
   if (tree_has_type(r))
      return tree_type(r);

   switch (tree_subkind(r)) {
   case RANGE_ERROR:
      {
         type_t type = type_new(T_NONE);
         tree_set_type(r, type);
         return type;
      }
   case RANGE_EXPR:
      {
         type_t type = _solve_types(tab, tree_value(r));
         tree_set_type(r, type);
         return type;
      }
   case RANGE_TO:
   case RANGE_DOWNTO:
      {
         tree_t left = tree_left(r);
         tree_t right = tree_right(r);

         type_t ltype;
         if (is_unambiguous(left))
            ltype = _solve_types(tab, left);
         else
            ltype = try_solve_type(tab, left);

         type_t rtype;
         if (is_unambiguous(right))
            rtype = _solve_types(tab, right);
         else
            rtype = try_solve_type(tab, right);

         if (ltype != NULL)
            rtype = solve_types(tab, right, ltype);
         else if (rtype != NULL)
            ltype = solve_types(tab, left, rtype);
         else {
            // This relies on try_solve_type filling the empty type set
            // with the possible types of the left or right sides
            ltype = _solve_types(tab, left);
            rtype = _solve_types(tab, right);
         }

         tree_set_type(r, ltype);
         return ltype;
      }
   default:
      fatal_trace("invalid range subkind");
   }
}

static type_t try_solve_type(nametab_t *tab, tree_t expr)
{
   switch (tree_kind(expr)) {
   case T_FCALL:
   case T_PROT_FCALL:
      return try_solve_fcall(tab, expr);
   case T_STRING:
      return try_solve_string(tab, expr);
   case T_AGGREGATE:
      return try_solve_aggregate(tab, expr);
   case T_REF:
      return try_solve_ref(tab, expr);
   case T_LITERAL:
      return try_solve_literal(tab, expr);
   case T_OPEN:
      return try_solve_open(tab, expr);
   case T_ATTR_REF:
      return try_solve_attr_ref(tab, expr);
   default:
      fatal_trace("cannot solve types for %s", tree_kind_str(tree_kind(expr)));
   }
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
   case T_STRING:
      return solve_string(tab, expr);
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
   case T_EXTERNAL_NAME:
      return solve_external_name(tab, expr);
   case T_PROT_REF:
      return solve_prot_ref(tab, expr);
   case T_COND_VALUE:
      return solve_cond_value(tab, expr);
   case T_COND_EXPR:
      return solve_cond_expr(tab, expr);
   case T_VIEW_ELEMENT:
      return solve_view_element(tab, expr);
   case T_INERTIAL:
      return solve_inertial(tab, expr);
   default:
      fatal_trace("cannot solve types for %s", tree_kind_str(tree_kind(expr)));
   }

   return type_new(T_NONE);
}

type_t solve_types(nametab_t *tab, tree_t expr, type_t constraint)
{
   type_set_push(tab);
   type_set_add(tab, constraint, NULL);
   type_t type = _solve_types(tab, expr);
   type_set_pop(tab);
   return type;
}

type_t solve_known_subtype(nametab_t *tab, tree_t expr, type_t constraint)
{
   assert(constraint != NULL);

   type_set_push(tab);
   tab->top_type_set->known_subtype = true;

   type_set_add(tab, constraint, NULL);

   type_t type = _solve_types(tab, expr);

   type_set_pop(tab);
   return type;
}

type_t solve_target(nametab_t *tab, tree_t target, tree_t value)
{
   type_t value_type = NULL;
   if (tree_kind(target) == T_AGGREGATE) {
      type_set_push(tab);
      tab->top_type_set->pred = type_is_composite;

      value_type = _solve_types(tab, value);

      type_set_pop(tab);
   }

   return solve_types(tab, target, value_type);
}

type_t solve_condition(nametab_t *tab, tree_t *expr, type_t constraint)
{
   type_set_push(tab);
   type_set_add(tab, constraint, NULL);

   type_t type;
   if (standard() >= STD_08) {
      if (is_unambiguous(*expr))
         type = _solve_types(tab, *expr);
      else
         type = try_solve_type(tab, *expr);

      type_t boolean = std_type(NULL, STD_BOOLEAN);
      if (type == NULL || !type_eq(type, boolean)) {
         ident_t cconv = well_known(W_OP_CCONV);
         const symbol_t *sym = symbol_for(tab->top_scope, cconv);
         if (sym != NULL) {
            for (int i = 0; i < sym->ndecls; i++) {
               const decl_t *dd = get_decl(sym, i);

               tree_t sub = dd->tree;
               if (dd->kind == T_ALIAS && !(sub = get_aliased_subprogram(sub)))
                  continue;

               if ((dd->mask & N_FUNC) && tree_ports(sub) == 1) {
                  type_t p0_type = tree_type(tree_port(sub, 0));
                  type_set_add(tab, p0_type, dd->tree);
               }
            }
         }

         if (type == NULL)
            type = _solve_types(tab, *expr);

         const bool do_conversion =
            !type_eq(type, boolean) &&
            tab->top_type_set->members.count > 0
            && type_set_contains(tab, type);

         if (do_conversion) {
            tree_t fcall = tree_new(T_FCALL);
            tree_set_loc(fcall, tree_loc(*expr));
            tree_set_ident(fcall, cconv);
            add_param(fcall, *expr, P_POS, NULL);

            type = solve_fcall(tab, fcall);
            *expr = fcall;
         }
      }
   }
   else
      type = _solve_types(tab, *expr);

   type_set_pop(tab);
   return type;
}

type_t solve_psl_condition(nametab_t *tab, tree_t expr)
{
   type_set_push(tab);

   type_set_add(tab, std_type(NULL, STD_BOOLEAN), NULL);
   type_set_add(tab, std_type(NULL, STD_BIT), NULL);
   type_set_add(tab, ieee_type(IEEE_STD_ULOGIC), NULL);

   type_t type = _solve_types(tab, expr);

   type_set_pop(tab);
   return type;
}
