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
#include "array.h"
#include "common.h"
#include "diag.h"
#include "hash.h"
#include "lib.h"
#include "names.h"
#include "opt.h"
#include "phase.h"
#include "type.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct scope scope_t;
typedef struct type_set type_set_t;
typedef struct _spec spec_t;
typedef struct _sym_chunk sym_chunk_t;
typedef struct _lazy_sym lazy_sym_t;

typedef enum {
   O_IDLE,
   O_POS,
   O_NAMED,
} overload_state_t;

typedef A(tree_t) tree_list_t;
typedef A(type_t) type_list_t;

typedef struct {
   ident_t           name;
   tree_t            tree;
   tree_list_t       candidates;
   tree_list_t       params;
   overload_state_t  state;
   nametab_t        *nametab;
   bool              error;
   bool              trace;
   type_t            signature;
   tree_t            prefix;
   unsigned          initial;
   unsigned          nactuals;
} overload_t;

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

typedef symbol_t *(*lazy_fn_t)(scope_t *, ident_t, void *);
typedef void (*formal_fn_t)(diag_t *, ident_t, void *);
typedef void (*make_visible_t)(scope_t *, ident_t, tree_t);

typedef struct _lazy_sym {
   lazy_sym_t *next;
   lazy_fn_t   fn;
   void       *ctx;
} lazy_sym_t;

struct scope {
   scope_t       *parent;
   sym_chunk_t    symbols;
   sym_chunk_t   *sym_tail;
   hash_t        *lookup;
   hash_t        *gmap;
   spec_t        *specs;
   overload_t    *overload;
   formal_kind_t  formal_kind;
   formal_fn_t    formal_fn;
   void          *formal_arg;
   ident_t        prefix;
   tree_t         container;
   bool           suppress;
   lazy_sym_t    *lazy;
   tree_list_t    imported;
   scope_t       *chain;
};

struct nametab {
   scope_t    *top_scope;
   type_set_t *top_type_set;
   tree_t      std;
};

typedef enum {
   TS_CONDITION_CONVERSION = (1 << 0)
} type_set_flags_t;

typedef struct {
   type_t type;
   tree_t src;
} tracked_type_t;

typedef A(tracked_type_t) tracked_type_list_t;

struct type_set {
   tracked_type_list_t  members;
   type_set_t          *down;
   type_set_flags_t     flags;
};

static type_t _solve_types(nametab_t *tab, tree_t expr);
static bool can_call_no_args(nametab_t *tab, tree_t decl);
static bool is_forward_decl(tree_t decl, tree_t existing);
static bool denotes_same_object(tree_t a, tree_t b);
static void make_visible_slow(scope_t *s, ident_t name, tree_t decl);

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
                              bool (*pred)(type_t), const char *hint)
{
   if (tab->top_type_set == NULL || tab->top_type_set->members.count == 0)
      return;

   LOCAL_TEXT_BUF tb = tb_new();

   int poss = 0;
   for (unsigned n = 0; n < tab->top_type_set->members.count; n++) {
      tracked_type_t tt = tab->top_type_set->members.items[n];

      if (type_is_none(tt.type))
         continue;
      else if (pred == NULL || (*pred)(tt.type)) {
         if (n > 0 && n + 1 == tab->top_type_set->members.count)
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

   for (unsigned i = 0; i < tab->top_type_set->members.count; i++) {
      if (type_eq(tab->top_type_set->members.items[i].type, t))
         return;
   }

   APUSH(tab->top_type_set->members, ((tracked_type_t){t, src}));
}

static bool type_set_restrict(nametab_t *tab, bool (*pred)(type_t))
{
   if (tab->top_type_set == NULL)
      return false;

   int j = 0;
   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      tracked_type_t tt = tab->top_type_set->members.items[i];
      if ((*pred)(tt.type))
         tab->top_type_set->members.items[j++] = tt;
   }
   ATRIM(tab->top_type_set->members, j);

   return j > 0;
}

static int type_set_satisfies(nametab_t *tab, bool (*pred)(type_t), type_t *out)
{
   if (tab->top_type_set == NULL)
      return 0;

   int count = 0;
   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      tracked_type_t tt = tab->top_type_set->members.items[i];
      if ((*pred)(tt.type)) {
         *out = tt.type;
         count++;
      }
   }

   return count;
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

static bool type_set_error(nametab_t *tab)
{
   if (tab->top_type_set == NULL)
      return false;

   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      if (type_is_none(tab->top_type_set->members.items[i].type))
         return true;
   }

   return false;
}

static bool type_set_contains(nametab_t *tab, type_t type)
{
   if (tab->top_type_set == NULL || tab->top_type_set->members.count == 0)
      return true;

   for (int i = 0; i < tab->top_type_set->members.count; i++) {
      type_t member = tab->top_type_set->members.items[i].type;
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
         || kind == T_PROC_INST
         || kind == T_FUNC_INST;
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

   if (s->chain) free_scope(s->chain);

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

void map_generic_type(nametab_t *tab, type_t generic, type_t actual)
{
   assert(type_kind(generic) == T_GENERIC);

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128);

   hash_put(tab->top_scope->gmap, generic, actual);
}

void map_generic_subprogram(nametab_t *tab, tree_t decl, tree_t actual)
{
   assert(is_subprogram(actual));

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128);

   hash_put(tab->top_scope->gmap, decl, actual);
}

void map_generic_package(nametab_t *tab, tree_t inst)
{
   assert(tree_kind(inst) == T_PACK_INST);

   if (tab->top_scope->gmap == NULL)
      tab->top_scope->gmap = hash_new(128);

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
      else {
         for (lazy_sym_t *it = s->lazy; it; it = it->next) {
            if ((sym = (*it->fn)(s, name, it->ctx)))
               return sym;
         }
      }
   } while (s->formal_kind != F_RECORD && (s = s->parent));

   return NULL;
}

static const decl_t *get_decl(const symbol_t *sym, unsigned nth)
{
   assert(nth < sym->ndecls);
   if (nth < INLINE_DECLS)
      return &(sym->decls[nth]);
   else
      return &(sym->overflow[nth - INLINE_DECLS]);
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
      return N_TYPE;
   case T_GENERIC_DECL:
      switch (class_of(t)) {
      case C_TYPE: return N_TYPE;
      case C_FUNCTION: return N_FUNC;
      case C_PROCEDURE: return N_PROC;
      default: return N_OBJECT;
      }
   case T_ALIAS:
      {
         if (tree_has_type(t)) {
            const type_kind_t kind = type_kind(tree_type(t));
            if (kind == T_FUNC) return N_FUNC;
            else if (kind == T_PROC) return N_PROC;
         }

         switch (class_of(tree_value(t))) {
         case C_TYPE: case C_SUBTYPE: return N_TYPE;
         case C_FUNCTION: return N_FUNC;
         case C_PROCEDURE: return N_PROC;
         case C_LABEL: return N_LABEL;
         default: return N_OBJECT;
         }

         return 0;
      }
   default:
      return 0;
   }
}

static bool signature_has_error(tree_t sub)
{
   type_t type = tree_type(sub);
   if (type_is_none(type))
      return true;

   assert(type_is_subprogram(type));

   const int nparams = type_params(type);
   bool error = false;

   for (int i = 0; !error && i < nparams; i++) {
      if (type_is_none(type_param(type, i)))
         return true;
   }

   if (type_kind(type) == T_FUNC && type_is_none(type_result(type)))
      return true;

   return false;
}

static void add_type_literals(scope_t *s, tree_t decl, make_visible_t fn)
{
   tree_t type_decl = aliased_type_decl(decl);
   if (type_decl == NULL)
      return;

   type_t type = tree_type(type_decl);
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

   for (int i = 0; i < sym->ndecls; i++) {
      decl_t *dd = (i < INLINE_DECLS)
         ? &(sym->decls[i]) : &(sym->overflow[i - INLINE_DECLS]);

      if (dd->tree == decl)
         return sym;   // Ignore duplicates
      else if (dd->visibility == HIDDEN || dd->visibility == ATTRIBUTE)
         continue;
      else if (tkind == T_LIBRARY && dd->kind == T_LIBRARY
               && tree_ident(decl) == name)
         return sym;   // Ignore redundant library declarations
      else if (dd->kind == T_TYPE_DECL && tkind == T_PROT_BODY
               && type_is_protected(tree_type(dd->tree)))
         continue;
      else if ((!overload || dd->visibility != OVERLOAD) && kind == DIRECT) {
         if (dd->origin == s && is_forward_decl(decl, dd->tree)) {
            if ((mask & N_SUBPROGRAM) && tree_subkind(dd->tree) == S_FOREIGN) {
               // Ignore redundant bodies of foreign subprograms
               return sym;
            }
            else {
               // Replace forward declaration in same region with full
               // definition
               dd->visibility = HIDDEN;
            }
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
            diag_emit(d);
            return sym;
         }
         else if (is_design_unit(decl))
            ;   // Design unit is top level so cannot hide anything
         else if (!overload && dd->visibility == DIRECT
                  && opt_get_int(OPT_WARN_HIDDEN)) {
            diag_t *d = diag_new(DIAG_WARN, tree_loc(decl));
            diag_printf(d, "declaration of %s %s hides %s %s",
                        class_str(class_of(decl)), istr(name),
                        class_str(class_of(dd->tree)), istr(name));
            diag_hint(d, tree_loc(dd->tree), "earlier declaration of %s is "
                      "hidden", istr(name));
            diag_hint(d, tree_loc(decl), "hidden by this declaration");
            diag_emit(d);
         }
         dd->visibility = HIDDEN;
      }
      else if (!overload && kind == POTENTIAL && dd->visibility == DIRECT)
         kind = HIDDEN;
      else if (kind == POTENTIAL && denotes_same_object(dd->tree, decl))
         return sym;   // Same object visible through different aliases
      else if (dd->origin == origin && (dd->mask & mask & N_SUBPROGRAM)
               && is_subprogram(decl) && is_subprogram(dd->tree)
               && type_eq(tree_type(dd->tree), type)
               && (tree_flags(dd->tree) & TREE_F_PREDEFINED)) {
         // Allow pre-defined operators be to hidden by
         // user-defined subprograms in the same region
         tree_set_flag(dd->tree, TREE_F_HIDDEN);
         dd->visibility = HIDDEN;
      }
      else if (is_forward_decl(decl, dd->tree)) {
         if ((dd->mask & N_SUBPROGRAM)
             && (tree_flags(dd->tree) & TREE_F_FOREIGN)) {
            // Hide bodies of subprograms declared with 'FOREIGN attribute
            return sym;
         }
         else
            dd->visibility = HIDDEN;
      }
      else if (overload && (mask & dd->mask & (N_SUBPROGRAM | N_OBJECT))
               && kind == DIRECT && type_eq(type, tree_type(dd->tree))) {
         if (dd->origin != s) {
            // LRM 93 section 10.3 on visibility specifies that if two
            // declarations are homographs then the one in the inner scope
            // hides the one in the outer scope
            dd->visibility = HIDDEN;
         }
         else if (signature_has_error(decl))
            ;  // Ignore cascading errors
         else if ((dd->kind == T_FUNC_BODY && tkind == T_FUNC_BODY)
                  || (dd->kind == T_PROC_BODY && tkind == T_PROC_BODY)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(decl));
            diag_printf(d, "duplicate subprogram body %s", type_pp(type));
            diag_hint(d, tree_loc(decl), "duplicate definition here");
            diag_hint(d, tree_loc(dd->tree), "previous definition was here");
            diag_emit(d);
            return sym;
         }
         else if (dd->origin == origin && !type_is_none(type)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(decl));
            diag_printf(d, "%s already declared in this region", type_pp(type));
            diag_hint(d, tree_loc(dd->tree), "previous declaration was here");
            diag_hint(d, tree_loc(decl), "duplicate declaration");
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

   if (kind == DIRECT)
      add_type_literals(s, decl, make_visible_slow);

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
         make_visible(s, tree_ident(dd->tree), dd->tree, POTENTIAL, dd->origin);
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

   tree_t unit = lib_get(lib, name);
   if (unit == NULL)
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

   add_type_literals(s, d, make_visible_fast);
}

static ident_t unit_bare_name(tree_t unit)
{
   ident_t unit_name = tree_ident(unit);
   return ident_rfrom(unit_name, '.') ?: unit_name;
}

static scope_t *private_scope_for(nametab_t *tab, tree_t unit)
{
   static hash_t *cache = NULL;
   if (cache == NULL)
      cache = hash_new(128);

   const tree_kind_t kind = tree_kind(unit);

   type_t type = NULL;
   bool cacheable = true;
   void *key = unit;
   if (kind == T_LIBRARY)
      key = tree_ident(unit);   // Tree pointer is not stable
   else if (kind == T_PARAM_DECL || kind == T_VAR_DECL) {
      key = type = tree_type(unit);
      assert(type_is_protected(type));
      cacheable = type_frozen(type);
   }
   else {
      assert(is_container(unit));
      cacheable = tree_frozen(unit);
   }

   scope_t *s = NULL;
   if (cacheable && (s = hash_get(cache, key)))
      return s;
   else {
      for (scope_t *ss = tab->top_scope; ss; ss = ss->parent) {
         for (s = ss->chain; s; s = s->chain) {
            if (s->container == unit)
               return s;
         }
      }
   }

   s = xcalloc(sizeof(scope_t));
   s->lookup    = hash_new(128);
   s->sym_tail  = &(s->symbols);
   s->container = unit;

   if (kind == T_LIBRARY)
      make_library_visible(s, lib_require(tree_ident(unit)));
   else if (type != NULL) {
      const int ndecls = type_decls(type);
      for (int i = 0; i < ndecls; i++) {
         tree_t d = type_decl(type, i);
         make_visible_fast(s, tree_ident(d), d);
      }
   }
   else {
      // For package instances do not export the names declared only in
      // the body
      const int ndecls =
         tree_decls(kind == T_PACK_INST ? tree_ref(unit) : unit);

      for (int i = 0; i < ndecls; i++) {
         tree_t d = tree_decl(unit, i);
         const tree_kind_t dkind = tree_kind(d);
         if (dkind == T_ALIAS || dkind == T_TYPE_DECL) {
            // Handle special cases: an alias may make the same
            // enumeration literal visible multiple times and a type
            // declaration may hide an earlier incomplete type
            make_visible_slow(s, tree_ident(d), d);
         }
         else
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
                  if (value != NULL && tree_kind(value) == T_REF) {
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

   if (cacheable)
      hash_put(cache, key, s);
   else {
      s->chain = tab->top_scope->chain;
      tab->top_scope->chain = s;
   }

   return s;
}

static bool is_forward_decl(tree_t decl, tree_t existing)
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

   const symbol_t *sym = symbol_for(region, name);
   if (sym == NULL)
      return NULL;

   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->origin == region && is_forward_decl(decl, dd->tree))
         return dd->tree;
   }

   return NULL;
}

void insert_name(nametab_t *tab, tree_t decl, ident_t alias)
{
   make_visible_slow(tab->top_scope, alias ?: tree_ident(decl), decl);
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
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(spec));
         diag_printf(d, "duplicate specification for instance %s", istr(ident));
         diag_hint(d, tree_loc((*p)->tree), "previous specification was here");
         diag_hint(d, tree_loc(spec), "duplicate specification here");
         diag_emit(d);
         return;
      }
   }
   *p = s;
}

type_t resolve_type(nametab_t *tab, type_t incomplete)
{
   assert(type_kind(incomplete) == T_INCOMPLETE);

   ident_t name = ident_rfrom(type_ident(incomplete), '.');

   const symbol_t *sym = symbol_for(tab->top_scope, name);
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->kind == T_TYPE_DECL) {
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
   const symbol_t *sym = symbol_for(tab->top_scope, name);
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
   if (standard() >= STD_08) {
      // According LRM 08 section 12.3 two declarations are not
      // homographs if they denote different named entities

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
   }

   return a == b;
}

static const symbol_t *iterate_symbol_for(nametab_t *tab, ident_t name)
{
   for (scope_t *s = tab->top_scope, *ss = NULL; ; s = ss, ss = NULL) {
      ident_t next = ident_walk_selected(&name);
      const symbol_t *sym = symbol_for(s, next);
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

static void hint_for_typo(nametab_t *tab, diag_t *d, ident_t name,
                          name_mask_t filter)
{
   if (ident_runtil(name, '.') != name)
      return;   // Ignore selected names

   const symbol_t *best = NULL;
   int bestd = INT_MAX;

   for (scope_t *s = tab->top_scope; s != NULL; s = s->parent) {
      for (sym_chunk_t *chunk = &(s->symbols); chunk; chunk = chunk->chain) {
         for (int i = 0; i < chunk->count; i++) {
            if (chunk->symbols[i].mask & filter) {
               const int d = ident_distance(chunk->symbols[i].name, name);
               if (d < bestd) {
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

tree_t resolve_name(nametab_t *tab, const loc_t *loc, ident_t name)
{
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
         diag_printf(d, "no visible declaration for %s", istr(name));
         hint_for_typo(tab, d, name, N_OBJECT | N_TYPE);
         diag_emit(d);
      }

      // Suppress further errors for this name
      local_symbol_for(tab->top_scope, name)->mask |= N_ERROR;

      return NULL;
   }
   else if (sym->ndecls == 1)
      return get_decl(sym, 0)->tree;

   // Check if all but one declartion is hidden
   int hidden = 0, overload = 0, subprograms = 0;
   tree_t result = NULL;
   for (int i = 0; i < sym->ndecls; i++) {
      const decl_t *dd = get_decl(sym, i);
      if (dd->visibility == HIDDEN || dd->visibility == ATTRIBUTE)
         hidden++;
      else if (dd->kind == T_PROT_BODY)
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

   tree_kind_t what = T_LAST_TREE_KIND;
   for (unsigned i = 0; i < sym->ndecls; i++) {
      const tree_kind_t kind = tree_kind(get_decl(sym, i)->tree);
      if (what == T_LAST_TREE_KIND)
         what = kind;
      else if (what != kind)
         what = T_REF;
   }

   if (overload > 0) {
      // Use the context to determine the correct overload

      SCOPED_A(tree_t) m = AINIT;
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == OVERLOAD)
            APUSH(m, dd->tree);
      }

      if (m.count > 1 && subprograms > 0) {
         unsigned wptr = 0;
         for (int i = 0; i < m.count; i++) {
            if (is_subprogram(m.items[i])) {
               // Remove subprograms that cannot be called with zero
               // arguments
               if (!can_call_no_args(tab, m.items[i]))
                  continue;
            }

            m.items[wptr++] = m.items[i];
         }
         ATRIM(m, wptr);
      }

      if (m.count > 1 && tab->top_type_set != NULL) {
         unsigned wptr = 0;
         for (unsigned i = 0; i < m.count; i++) {
            if (class_has_type(class_of(m.items[i]))) {
               type_t type = tree_type(m.items[i]);
               if (type_kind(type) == T_FUNC)
                  type = type_result(type);

               if (type_set_contains(tab, type))
                  m.items[wptr++] = m.items[i];
            }
         }
         if (wptr > 0) ATRIM(m, wptr);
      }

      if (m.count == 1)
         return m.items[0];
      else if (type_set_error(tab))
         return NULL;  // Suppress cascading errors
   }

   if (sym->mask & N_ERROR)
      return NULL;    // Was an earlier error

   diag_t *d = diag_new(DIAG_ERROR, loc);
   if (overload == 0)
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
      if (type != NULL)
         tb_printf(tb, " as %s", type_pp(tree_type(dd->tree)));

      if (dd->origin->container != NULL)
         tb_printf(tb, " from %s", istr(tree_ident(dd->origin->container)));

      diag_hint(d, tree_loc(dd->tree), "%s", tb_get(tb));
   }

   diag_hint(d, loc, "use of name %s here", istr(name));
   diag_emit(d);

   return NULL;
}

tree_t resolve_subprogram_name(nametab_t *tab, tree_t ref, type_t constraint)
{
   assert(tree_kind(ref) == T_REF);

   const bool allow_enum =
      constraint != NULL && type_kind(constraint) == T_FUNC
      && type_params(constraint) == 0;

   tree_t decl = NULL;
   const symbol_t *sym = iterate_symbol_for(tab, tree_ident(ref));
   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (dd->mask & N_SUBPROGRAM) {
            if (constraint == NULL) {
               decl = dd->tree;   // TODO: ambiguous?
               break;
            }
            else {
               type_t signature = tree_type(dd->tree);
               if (type_eq_map(constraint, signature, tab->top_scope->gmap)) {
                  decl = dd->tree;
                  break;
               }
            }
         }
         else if (allow_enum && dd->kind == T_ENUM_LIT
                  && type_eq(type_result(constraint), tree_type(dd->tree))) {
            decl = dd->tree;
            break;
         }
      }
   }

   if (decl == NULL) {
      const char *signature = strchr(type_pp(constraint), '[');
      error_at(tree_loc(ref), "no visible subprogram%s %s matches "
               "signature %s", allow_enum ? " or enumeration literal" : "",
               istr(tree_ident(ref)), signature);
   }

   return decl;
}

static tree_t resolve_ref(nametab_t *tab, tree_t ref)
{
   type_t constraint;
   if (type_set_uniq(tab, &constraint) && type_is_subprogram(constraint)) {
      // Reference to subprogram or enumeration literal with signature
      return resolve_subprogram_name(tab, ref, constraint);
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
      bool error;
      if (lib_name == unit_name) {
         make_library_visible(tab->top_scope, lib);
         return;
      }
      else if (unit_name == well_known(W_ERROR))
         return;   // Was earlier parse error
      else if ((unit = lib_get_check_stale(lib, unit_name, &error)) == NULL) {
         error_at(tree_loc(use), "unit %s not found in library %s",
                  istr(unit_name), istr(ident_until(unit_name, '.')));
         return;
      }
      else if (error) {
         error_at(tree_loc(use), "design unit %s was analysed with errors",
                  istr(unit_name));
         return;
      }

      unit_name = tree_ident(unit);
   }

   ident_t tag = unit_name;
   if (tree_has_ident2(use))
      tag = ident_prefix(tag, tree_ident2(use), '.');

   if (tree_has_ident2(use)) {
      if (!is_package(unit)) {
         error_at(tree_loc(use), "design unit %s is not a package",
                  istr(unit_name));
         return;
      }
      else if (lib_import && is_uninstantiated_package(unit)) {
         error_at(tree_loc(use), "cannot use an uninstantiated package");
         return;
      }

      scope_t *s = private_scope_for(tab, unit);
      assert(s->container == unit);

      ident_t what = tree_ident2(use);
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
            error_at(tree_loc(use), "object %s not found in unit %s",
                     istr(what), istr(tree_ident(unit)));
            return;
         }

         merge_symbol(tab->top_scope, sym);

         // If this is an enumeration or physical type then also make
         // the literals visible
         // TODO: for VHDL-2008, also predefined functions
         for (int i = 0; i < sym->ndecls; i++) {
            const decl_t *dd = get_decl(sym, i);
            if (dd->kind == T_TYPE_DECL) {
               type_t type = tree_type(dd->tree);
               if (type_is_enum(type)) {
                  const int nlits = type_enum_literals(type);
                  for (int i = 0; i < nlits; i++) {
                     tree_t lit = type_enum_literal(type, i);
                     make_visible(tab->top_scope, tree_ident(lit), lit,
                                  POTENTIAL, s);
                  }
               }
               else if (type_is_physical(type)) {
                  const int nunits = type_units(type);
                  for (int i = 0; i < nunits; i++) {
                     tree_t u = type_unit(type, i);
                     make_visible(tab->top_scope, tree_ident(u), u,
                                  POTENTIAL, s);
                  }
               }
            }
         }

         merge_symbol(tab->top_scope, symbol_for(s, unit_bare_name(unit)));
      }
   }
   else {
      ident_t bare_name = unit_bare_name(unit);
      make_visible(tab->top_scope, bare_name, unit, POTENTIAL, tab->top_scope);
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
         insert_name(tab, tree_decl(container, i), NULL);
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

void insert_protected_decls(nametab_t *tab, type_t type)
{
   assert(type_is_protected(type));

   const int ndecls = type_decls(type);
   for (int i = 0; i < ndecls; i++)
      insert_name(tab, type_decl(type, i), NULL);
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

static tree_t get_container(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ALIAS:
      return get_container(tree_value(t));
   case T_REF:
      return get_container(tree_ref(t));
   case T_VAR_DECL:
   case T_PARAM_DECL:
      return type_is_protected(tree_type(t)) ? t : NULL;
   default:
      return is_container(t) ? t : NULL;
   }
}

static void begin_overload_resolution(overload_t *o)
{
   const symbol_t *sym = NULL;
   if (o->prefix != NULL) {
      tree_t container = get_container(o->prefix);
      if (container != NULL) {
         scope_t *scope = private_scope_for(o->nametab, container);
         sym = symbol_for(scope, o->name);
      }
   }
   else
      sym = iterate_symbol_for(o->nametab, o->name);

   if (sym != NULL) {
      for (int i = 0; i < sym->ndecls; i++) {
         const decl_t *dd = get_decl(sym, i);
         if (dd->visibility == HIDDEN)
            continue;
         else if (!(dd->mask & N_SUBPROGRAM))
            continue;

         tree_t next = dd->tree;
         if (dd->kind == T_ALIAS) {
            tree_t value = tree_value(next);
            if (tree_kind(value) != T_REF || !tree_has_ref(value))
               continue;

            next = tree_ref(value);
         }

         overload_add_candidate(o, next);
      }
   }

   // Remove any duplicates from aliases
   if (o->candidates.count > 1) {
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
   o->error   = type_set_error(o->nametab);

   if (o->initial == 0 && !o->error) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(o->tree));
      diag_printf(d, "no visible subprogram declaration for %s", istr(o->name));
      hint_for_typo(o->nametab, d, o->name, N_SUBPROGRAM);

      if (sym != NULL) {
         const bool hinted = diag_hints(d) > 0;
         for (int i = 0; i < sym->ndecls; i++) {
            const decl_t *dd = get_decl(sym, i);
            if ((dd->mask & N_SUBPROGRAM) && dd->visibility == HIDDEN)
               diag_hint(d, tree_loc(dd->tree), "subprogram %s is hidden",
                         type_pp(tree_type(dd->tree)));
         }

         if (!hinted)
            diag_hint(d, tree_loc(o->tree), "%s called here", istr(o->name));
      }

      diag_emit(d);
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

   // Prune candidates with more required arguments than supplied
   if (o->candidates.count > 1) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t d = o->candidates.items[i];
         int nrequired = 0;
         const int nports = tree_ports(d);
         for (int i = 0; i < nports; i++) {
            if (!tree_has_value(tree_port(d, i)))
               nrequired++;
         }

         if (o->nactuals < nrequired)
            overload_prune_candidate(o, i);
         else
            o->candidates.items[wptr++] = d;
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

   int count = 0;
   tree_t result = NULL;
   for (unsigned i = 0; i < o->candidates.count; i++) {
      tree_t d = o->candidates.items[i];
      if (tree_flags(d) & TREE_F_UNIVERSAL) {
         // LRM 08 section 9.3.6 implicit conversions only occur when
         // there is a unique numeric type in the context. If a univeral
         // operator is in the candidate list then the other candidates
         // can only be there because of implicit conversions.
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
      const loc_t *loc = tree_loc(o->tree);
      diag_t *d = diag_new(DIAG_ERROR, loc);
      diag_printf(d, "ambiguous %s %s",
                  ident_char(o->name, 0) == '"' ? "use of operator"
                  : (tree_kind(o->tree) == T_FCALL ? "call to function"
                     : "call to procedure"),
                  istr(o->name));

      for (unsigned i = 0; i < o->candidates.count; i++) {
         if (o->candidates.items[i]) {
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
         }
      }

      if (diag_hints(d) > 0)
         diag_hint(d, tree_loc(o->tree), "use of name %s here", istr(o->name));

      diag_emit(d);
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
                      type_pp(ts->members.items[i].type));
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
   o->nametab->top_scope->formal_kind = F_SUBPROGRAM;
   o->nametab->top_scope->overload = o;

   for (unsigned i = 0; i < o->candidates.count; i++) {
      if (o->candidates.items[i]) {
         const int nports = tree_ports(o->candidates.items[i]);
         for (int j = 0; j < nports; j++) {
            tree_t p = tree_port(o->candidates.items[i], j);
            make_visible_slow(o->nametab->top_scope, tree_ident(p), p);
         }
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
         type_set_add(o->nametab, tree_type(p), d);
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
            type_set_add(o->nametab, tree_type(p), o->candidates.items[i]);
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

   if (o->initial > 1 && tree_subkind(p) == P_POS) {
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

            if (matches == 0) {
               overload_prune_candidate(o, i);
               continue;
            }
         }

         o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);
   }
}

static void overload_restrict_argument_type(overload_t *o, tree_t p,
                                            bool (*pred)(type_t),
                                            const char *trace)
{
   assert(tree_kind(p) == T_PARAM);
   assert(o->state == O_IDLE);

   if (o->trace) {
      printf("%s: restrict argument ", istr(o->name));
      if (tree_subkind(p) == P_POS)
         printf("%d to", tree_pos(p));
      else if (tree_kind(tree_name(p)) == T_REF)
         printf("%s to ", istr(tree_ident(tree_name(p))));
      printf(" %s\n", trace);
   }

   if (o->initial > 1 && tree_subkind(p) == P_POS) {
      unsigned wptr = 0;
      for (unsigned i = 0; i < o->candidates.count; i++) {
         tree_t port = overload_find_port(o->candidates.items[i], p);
         if (port != NULL) {
            type_t ptype = tree_type(port);
            if (!(*pred)(ptype)) {
               overload_prune_candidate(o, i);
               continue;
            }
         }

         o->candidates.items[wptr++] = o->candidates.items[i];
      }
      ATRIM(o->candidates, wptr);
   }
}

void map_generic_box(nametab_t *tab, tree_t inst, tree_t g, unsigned pos)
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
   tree_set_subkind(map,  P_POS);
   tree_set_pos(map, pos);
   tree_set_value(map, ref);

   tree_add_genmap(inst, map);

   if (tree_has_ref(ref))
      map_generic_subprogram(tab, g, tree_ref(ref));
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

static bool is_unambiguous(tree_t t)
{
   if (tree_has_type(t))
      return true;

   const tree_kind_t kind = tree_kind(t);
   return kind == T_QUALIFIED
      || kind == T_ARRAY_REF
      || kind == T_ARRAY_SLICE
      || kind == T_TYPE_CONV
      || kind == T_ATTR_REF
      || kind == T_RECORD_REF
      || kind == T_ALL
      || (kind == T_REF && tree_has_ref(t));
}

static type_list_t possible_types(nametab_t *tab, tree_t value)
{
   tree_kind_t kind = tree_kind(value);
   type_list_t possible = AINIT;

   if (kind == T_REF || kind == T_FCALL) {
      ident_t name = tree_ident(value);
      const symbol_t *sym = symbol_for(tab->top_scope, name);
      if (sym != NULL) {
         for (int i = 0; i < sym->ndecls; i++) {
            const decl_t *dd = get_decl(sym, i);
            if (dd->visibility == HIDDEN)
               continue;

            type_t type1 = tree_type(dd->tree);
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
         }
      }
   }

   return possible;
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
      if (is_unambiguous(value)) {
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
         type_list_t possible = possible_types(tab, value);
         if (possible.count > 0)
            overload_restrict_argument(o, p, possible.items, possible.count);
         ACLEAR(possible);
      }
      else if (kind == T_AGGREGATE || kind == T_STRING) {
         // This argument must have composite type
         overload_restrict_argument_type(o, p, type_is_composite, "composite");
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

static type_t solve_string(nametab_t *tab, tree_t str)
{
   if (tree_has_type(str))
      return tree_type(str);

   // The type must be determinable soley from the context excluding the
   // literal itself but using the fact that the type must be a one
   // dimensional array of a character type

   type_t type = NULL;
   if (type_set_satisfies(tab, is_character_array, &type) != 1) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(str));
      diag_printf(d, "type of string literal cannot be determined "
                  "from the surrounding context");
      type_set_describe(tab, d, tree_loc(str), is_character_array,
                        "a one dimensional array of character type");
      diag_emit(d);

      type = type_new(T_NONE);
   }

   type_t elem = type_elem(type);
   const int nchars = tree_chars(str);
   for (int i = 0; i < nchars; i++)
      solve_types(tab, tree_char(str, i), elem);

   type_t sub = subtype_for_string(str, type);
   tree_set_type(str, sub);
   return sub;
}

static type_t solve_literal(nametab_t *tab, tree_t lit)
{
   if (tree_has_type(lit))
      return tree_type(lit);

   switch (tree_subkind(lit)) {
   case L_NULL:
      {
         type_t type;
         if (!type_set_uniq(tab, &type)) {
            type_set_restrict(tab, type_is_access);

            if (!type_set_uniq(tab, &type)) {
               error_at(tree_loc(lit), "invalid use of null expression");
               type = type_new(T_NONE);
            }
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
   type_t type = NULL;

   if (decl == NULL) {
      tree_set_type(ref, (type = type_new(T_NONE)));
      return type;
   }
   else if (!class_has_type(class_of(decl))) {
      error_at(tree_loc(ref), "invalid use of %s %s",
               class_str(class_of(decl)), istr(tree_ident(ref)));
      tree_set_ref(ref, NULL);
      tree_set_type(ref, (type = type_new(T_NONE)));
      return type;
   }

   if (tree_kind(decl) == T_ALIAS && !tree_has_type(decl))
      type = tree_type(tree_value(decl));
   else
      type = tree_type(decl);

   if (type_is_subprogram(type)) {
      type_t constraint;
      const bool want_ref =
         type_set_uniq(tab, &constraint) && type_is_subprogram(constraint);

      if (can_call_no_args(tab, decl) && !want_ref) {
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

   type_t elem_type;
   if (type_is_array(base_type))
      elem_type = type_elem(base_type);
   else
      elem_type = type_new(T_NONE);

   if (standard() >= STD_08 && type_kind(base_type) == T_SUBTYPE) {
      tree_t cons[MAX_CONSTRAINTS];
      const int ncon = pack_constraints(base_type, cons);
      if (ncon > 1) {
         assert(type_is_unconstrained(elem_type));

         type_t sub = type_new(T_SUBTYPE);
         type_set_base(sub, elem_type);
         for (int i = 1; i < ncon; i++)
            type_add_constraint(sub, cons[i]);

         elem_type = sub;
      }
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
   type_set_base(slice_type, base_type);
   type_add_constraint(slice_type, constraint);

   if (standard() >= STD_08 && type_kind(base_type) == T_SUBTYPE) {
      tree_t cons[MAX_CONSTRAINTS];
      const int ncon = pack_constraints(base_type, cons);
      for (int i = 1; i < ncon; i++)
         type_add_constraint(slice_type, cons[i]);
   }

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
      type = prefix_type ?: type_new(T_NONE);

      if (type != NULL && type_is_array(type)) {
         int dim = 0;
         if (nparams > 0) {
            tree_t pdim = tree_value(tree_param(aref, 0));
            if (tree_kind(pdim) == T_LITERAL) // XXX: what if real or physical?
               dim = tree_ival(pdim) - 1;
         }

         type = index_type_of(type, dim) ?: type_new(T_NONE);
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

   case ATTR_SUBTYPE:
   case ATTR_ELEMENT:
      // These should have been expanded by the parser so can only be
      // erroneous
      type = prefix_type;
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
               const symbol_t *sym = symbol_for(tab->top_scope, attr);
               if (sym != NULL) {
                  for (int i = 0; i < sym->ndecls; i++) {
                     const decl_t *dd = get_decl(sym, i);
                     if (dd->visibility != ATTRIBUTE)
                        continue;
                     else if (tree_class(dd->tree) == class
                              && tree_ident2(dd->tree) == id) {
                        a = dd->tree;
                        break;
                     }
                  }
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
   if (type_set_error(tab))
      type = type_new(T_NONE);
   else if (type_set_satisfies(tab, type_is_composite, &type) != 1) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(agg));
      diag_printf(d, "type of aggregate cannot be determined "
                  "from the surrounding context");
      type_set_describe(tab, d, tree_loc(agg), type_is_composite,
                        "a composite type");

      diag_emit(d);
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
               if (pos < nfields) {
                  tree_t f = type_field(type, pos);
                  type_set_add(tab, solve_field_subtype(type, f), f);
               }
               if (pos < 64) fmask |= (1 << pos);
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
                     if (pos < 64) fmask |= (1 << pos);
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
               if (!(fmask & (1 << i))) {
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
   }
   else {
      // All elements must be of the composite base type if this is a
      // one-dimensional array otherwise construct an array type with
      // n-1 dimensions.

      type_set_push(tab);

      type_t t0, t1 = NULL;
      const int ndims = dimension_of(type);
      if (ndims == 1) {
         type_t elem = type_elem(type);
         if (type_is_unconstrained(elem)) {
            tree_t cons[MAX_CONSTRAINTS];
            const int ncon = pack_constraints(type, cons);
            if (ncon > 1) {
               // Create a constrained subtype for the element
               type_t sub = type_new(T_SUBTYPE);
               type_set_base(sub, elem);
               for (int i = 1; i < ncon; i++)
                  type_add_constraint(sub, cons[i]);

               elem = sub;
            }
         }
         type_set_add(tab, (t0 = elem), NULL);

         if (standard() >= STD_08 && !type_is_composite(elem))
            type_set_add(tab, (t1 = type_base_recur(type)), NULL);
      }
      else
         type_set_add(tab, (t0 = array_aggregate_type(type, 1)), NULL);

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

         // Hack to avoid pushing/popping type set on each iteration
         ATRIM(tab->top_type_set->members, 0);
         type_set_add(tab, t0, NULL);
         type_set_add(tab, t1, NULL);
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
   if (type_set_satisfies(tab, type_is_access, &type) != 1) {
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
      type = type_access(type);
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

static type_t solve_external_name(nametab_t *tab, tree_t name)
{
   if (tree_has_type(name))
      return tree_type(name);

   assert(error_count() > 0);
   type_t none = type_new(T_NONE);
   tree_set_type(name, none);
   return none;
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

         if (is_unambiguous(left)) {
            type = _solve_types(tab, left);
            type_set_add(tab, type, left);
            _solve_types(tab, right);
         }
         else if (is_unambiguous(right)) {
            type = _solve_types(tab, right);
            type_set_add(tab, type, right);
            _solve_types(tab, left);
         }
         else if (tab->top_type_set->members.count > 0) {
            type = _solve_types(tab, left);
            _solve_types(tab, right);
         }
         else {
            type_list_t lposs = possible_types(tab, left);
            type_list_t rposs = possible_types(tab, right);

            for (int i = 0; i < lposs.count; i++) {
               for (int j = 0; j < rposs.count; j++) {
                  if (type_eq(lposs.items[i], rposs.items[j])) {
                     type_set_add(tab, lposs.items[i], left);
                     break;
                  }
               }
            }

            type = _solve_types(tab, left);
            solve_types(tab, right, type);

            ACLEAR(lposs);
            ACLEAR(rposs);
         }

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
   case T_EXTERNAL_NAME:
      return solve_external_name(tab, expr);
   case T_PROT_REF:
      return solve_prot_ref(tab, expr);
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

type_t solve_condition(nametab_t *tab, tree_t expr, type_t constraint)
{
   type_set_push(tab);
   type_set_add(tab, constraint, NULL);

   if (standard() >= STD_08)
      tab->top_type_set->flags |= TS_CONDITION_CONVERSION;

   type_t type = _solve_types(tab, expr);
   type_set_pop(tab);
   return type;
}
