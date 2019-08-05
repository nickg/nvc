//
//  Copyright (C) 2011-2019  Nick Gasson
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

#include "tree.h"
#include "util.h"
#include "array.h"
#include "object.h"
#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static const imask_t has_map[T_LAST_TREE_KIND] = {
   // T_ENTITY
   (I_IDENT | I_PORTS | I_GENERICS | I_CONTEXT | I_DECLS | I_STMTS | I_ATTRS),

   // T_ARCH
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_CONTEXT | I_REF | I_ATTRS),

   // T_PORT_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_SUBKIND | I_CLASS | I_ATTRS | I_FLAGS),

   // T_FCALL
   (I_IDENT | I_PARAMS | I_TYPE | I_REF | I_ATTRS | I_FLAGS),

   // T_LITERAL
   (I_SUBKIND | I_TYPE | I_IVAL | I_DVAL | I_CHARS | I_FLAGS),

   // T_SIGNAL_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_NETS | I_ATTRS | I_FLAGS),

   // T_VAR_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_ATTRS | I_FLAGS),

   // T_PROCESS
   (I_IDENT | I_DECLS | I_STMTS | I_TRIGGERS | I_ATTRS | I_FLAGS),

   // T_REF
   (I_IDENT | I_TYPE | I_REF | I_ATTRS | I_FLAGS),

   // T_WAIT
   (I_IDENT | I_VALUE | I_DELAY | I_TRIGGERS | I_ATTRS),

   // T_TYPE_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_OPS | I_ATTRS),

   // T_VAR_ASSIGN
   (I_IDENT | I_VALUE | I_TARGET | I_ATTRS),

   // T_PACKAGE
   (I_IDENT | I_DECLS | I_CONTEXT | I_ATTRS),

   // T_SIGNAL_ASSIGN
   (I_IDENT | I_TARGET | I_WAVES | I_REJECT | I_ATTRS),

   // T_QUALIFIED
   (I_IDENT | I_VALUE | I_TYPE),

   // T_ENUM_LIT
   (I_IDENT | I_TYPE | I_POS | I_ATTRS),

   // T_CONST_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_ATTRS | I_FLAGS),

   // T_FUNC_DECL
   (I_IDENT | I_VALUE | I_PORTS | I_TYPE | I_ATTRS | I_FLAGS),

   // T_ELAB
   (I_IDENT | I_DECLS | I_STMTS | I_CONTEXT | I_ATTRS),

   // T_AGGREGATE
   (I_TYPE | I_ASSOCS | I_FLAGS),

   // T_ASSERT
   (I_IDENT | I_VALUE | I_SEVERITY | I_MESSAGE | I_ATTRS),

   // T_ATTR_REF
   (I_NAME | I_VALUE | I_IDENT | I_PARAMS | I_TYPE | I_ATTRS),

   // T_ARRAY_REF
   (I_VALUE | I_PARAMS | I_TYPE | I_FLAGS | I_ATTRS),

   // T_ARRAY_SLICE
   (I_VALUE | I_TYPE | I_RANGES),

   // T_INSTANCE
   (I_IDENT | I_IDENT2 | I_PARAMS | I_GENMAPS | I_REF | I_CLASS | I_SPEC
    | I_ATTRS),

   // T_IF
   (I_IDENT | I_VALUE | I_STMTS | I_ELSES | I_ATTRS),

   // T_NULL
   (I_IDENT),

   // T_PACK_BODY
   (I_IDENT | I_DECLS | I_CONTEXT | I_ATTRS),

   // T_FUNC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE | I_ATTRS | I_FLAGS),

   // T_RETURN
   (I_IDENT | I_VALUE | I_ATTRS),

   // T_CASSIGN
   (I_IDENT | I_TARGET | I_CONDS | I_FLAGS),

   // T_WHILE
   (I_IDENT | I_VALUE | I_STMTS | I_ATTRS),

   // T_WAVEFORM
   (I_VALUE | I_DELAY),

   // T_ALIAS
   (I_IDENT | I_VALUE | I_TYPE | I_ATTRS),

   // T_FOR
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_RANGES | I_ATTRS),

   // T_ATTR_DECL
   (I_IDENT | I_TYPE | I_ATTRS),

   // T_ATTR_SPEC
   (I_IDENT | I_VALUE | I_IDENT2 | I_CLASS | I_ATTRS),

   // T_PROC_DECL
   (I_IDENT | I_PORTS | I_TYPE | I_ATTRS),

   // T_PROC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE | I_ATTRS),

   // T_EXIT
   (I_IDENT | I_VALUE | I_IDENT2 | I_ATTRS),

   // T_PCALL
   (I_IDENT | I_IDENT2 | I_PARAMS | I_REF | I_ATTRS),

   // T_CASE
   (I_IDENT | I_VALUE | I_ASSOCS | I_ATTRS),

   // T_BLOCK
   (I_IDENT | I_DECLS | I_STMTS | I_ATTRS),

   // T_COND
   (I_VALUE | I_WAVES | I_REJECT),

   // T_CONCAT
   (I_PARAMS | I_TYPE),

   // T_TYPE_CONV
   (I_PARAMS | I_TYPE | I_REF | I_FLAGS),

   // T_SELECT
   (I_IDENT | I_VALUE | I_ASSOCS | I_FLAGS),

   // T_COMPONENT
   (I_IDENT | I_PORTS | I_GENERICS | I_ATTRS),

   // T_IF_GENERATE
   (I_IDENT | I_VALUE | I_DECLS | I_STMTS | I_ATTRS),

   // T_FOR_GENERATE
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_REF | I_RANGES | I_ATTRS),

   // T_FILE_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FILE_MODE | I_ATTRS),

   // T_OPEN
   (I_TYPE),

   // T_FIELD_DECL
   (I_IDENT | I_TYPE | I_ATTRS),

   // T_RECORD_REF
   (I_IDENT | I_VALUE | I_TYPE | I_ATTRS),

   // T_ALL
   (I_VALUE | I_TYPE),

   // T_NEW
   (I_VALUE | I_TYPE),

   // T_CASSERT
   (I_IDENT | I_VALUE | I_SEVERITY | I_MESSAGE | I_FLAGS),

   // T_CPCALL
   (I_IDENT | I_IDENT2 | I_PARAMS | I_REF),

   // T_UNIT_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_NEXT
   (I_IDENT | I_VALUE | I_IDENT2 | I_ATTRS),

   // T_GENVAR
   (I_IDENT | I_TYPE | I_ATTRS),

   // T_PARAM
   (I_VALUE | I_POS | I_SUBKIND | I_NAME),

   // T_ASSOC
   (I_VALUE | I_POS | I_NAME | I_RANGES | I_SUBKIND),

   // T_USE
   (I_IDENT | I_IDENT2),

   // T_HIER
   (I_IDENT | I_SUBKIND | I_IDENT2 | I_ATTRS),

   // T_SPEC
   (I_IDENT | I_IDENT2 | I_VALUE),

   // T_BINDING
   (I_PARAMS | I_GENMAPS | I_IDENT | I_IDENT2 | I_CLASS),

   // T_LIBRARY
   (I_IDENT),

   // T_DESIGN_UNIT
   (I_CONTEXT),

   // T_CONFIGURATION
   (I_IDENT | I_IDENT2 | I_DECLS),

   // T_PROT_BODY
   (I_IDENT | I_TYPE | I_DECLS | I_ATTRS),

   // T_CONTEXT
   (I_CONTEXT | I_IDENT),

   // T_CTXREF
   (I_IDENT | I_REF),

   // T_CONSTRAINT
   (I_SUBKIND | I_RANGES),

   // T_BLOCK_CONFIG
   (I_DECLS | I_IDENT | I_VALUE | I_RANGES),

   // T_PRAGMA
   (I_TEXT),
};

static const char *kind_text_map[T_LAST_TREE_KIND] = {
   "T_ENTITY",       "T_ARCH",          "T_PORT_DECL",  "T_FCALL",
   "T_LITERAL",      "T_SIGNAL_DECL",   "T_VAR_DECL",   "T_PROCESS",
   "T_REF",          "T_WAIT",          "T_TYPE_DECL",  "T_VAR_ASSIGN",
   "T_PACKAGE",      "T_SIGNAL_ASSIGN", "T_QUALIFIED",  "T_ENUM_LIT",
   "T_CONST_DECL",   "T_FUNC_DECL",     "T_ELAB",       "T_AGGREGATE",
   "T_ASSERT",       "T_ATTR_REF",      "T_ARRAY_REF",  "T_ARRAY_SLICE",
   "T_INSTANCE",     "T_IF",            "T_NULL",       "T_PACK_BODY",
   "T_FUNC_BODY",    "T_RETURN",        "T_CASSIGN",    "T_WHILE",
   "T_WAVEFORM",     "T_ALIAS",         "T_FOR",        "T_ATTR_DECL",
   "T_ATTR_SPEC",    "T_PROC_DECL",     "T_PROC_BODY",  "T_EXIT",
   "T_PCALL",        "T_CASE",          "T_BLOCK",      "T_COND",
   "T_CONCAT",       "T_TYPE_CONV",     "T_SELECT",     "T_COMPONENT",
   "T_IF_GENERATE",  "T_FOR_GENERATE",  "T_FILE_DECL",  "T_OPEN",
   "T_FIELD_DECL",   "T_RECORD_REF",    "T_ALL",        "T_NEW",
   "T_CASSERT",      "T_CPCALL",        "T_UNIT_DECL",  "T_NEXT",
   "T_GENVAR",       "T_PARAM",         "T_ASSOC",      "T_USE",
   "T_HIER",         "T_SPEC",          "T_BINDING",    "T_LIBRARY",
   "T_DESIGN_UNIT",  "T_CONFIGURATION", "T_PROT_BODY",  "T_CONTEXT",
   "T_CTXREF",       "T_CONSTRAINT",    "T_BLOCK_CONFIG", "T_PRAGMA",
};

static const change_allowed_t change_allowed[] = {
   { T_REF,         T_FCALL         },
   { T_REF,         T_PCALL         },
   { T_ARRAY_REF,   T_FCALL         },
   { T_FCALL,       T_ARRAY_REF     },
   { T_FCALL,       T_PCALL         },
   { T_FCALL,       T_TYPE_CONV     },
   { T_REF,         T_RECORD_REF    },
   { T_ARRAY_REF,   T_ARRAY_SLICE   },
   { T_ASSERT,      T_CASSERT       },
   { T_DESIGN_UNIT, T_ENTITY        },
   { T_DESIGN_UNIT, T_PACKAGE       },
   { T_DESIGN_UNIT, T_PACK_BODY     },
   { T_DESIGN_UNIT, T_ARCH          },
   { T_DESIGN_UNIT, T_CONFIGURATION },
   { T_DESIGN_UNIT, T_CONTEXT       },
   { T_FUNC_DECL,   T_FUNC_BODY     },
   { T_PROC_DECL,   T_PROC_BODY     },
   { T_REF,         T_ARRAY_SLICE   },
   { T_FCALL,       T_CPCALL        },
   { T_REF,         T_CPCALL        },
   { T_ATTR_REF,    T_ARRAY_REF     },
   { -1,            -1              }
};

struct tree {
   object_t object;
};

static const tree_kind_t stmt_kinds[] = {
   T_PROCESS, T_WAIT,        T_VAR_ASSIGN,   T_SIGNAL_ASSIGN,
   T_ASSERT,  T_INSTANCE,    T_IF,           T_NULL,
   T_RETURN,  T_CASSIGN,     T_WHILE,        T_FOR,
   T_EXIT,    T_PCALL,       T_CASE,         T_BLOCK,
   T_SELECT,  T_IF_GENERATE, T_FOR_GENERATE, T_CPCALL,
   T_CASSERT, T_NEXT,        T_PRAGMA,
};

static tree_kind_t expr_kinds[] = {
   T_FCALL,     T_LITERAL,   T_REF,       T_QUALIFIED,
   T_AGGREGATE, T_ATTR_REF,  T_ARRAY_REF, T_ARRAY_SLICE,
   T_CONCAT,    T_TYPE_CONV, T_OPEN,      T_RECORD_REF,
   T_ALL,       T_NEW
};

static tree_kind_t decl_kinds[] = {
   T_PORT_DECL,  T_SIGNAL_DECL, T_VAR_DECL,   T_TYPE_DECL,
   T_CONST_DECL, T_FUNC_DECL,   T_FUNC_BODY,  T_ALIAS,
   T_ATTR_DECL,  T_ATTR_SPEC,   T_PROC_DECL,  T_PROC_BODY,
   T_COMPONENT,  T_FILE_DECL,   T_FIELD_DECL, T_UNIT_DECL,
   T_GENVAR,     T_HIER,        T_SPEC,       T_BINDING,
   T_USE,        T_PROT_BODY,   T_BLOCK_CONFIG
};

object_class_t tree_object = {
   .name           = "tree",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_TREE,
   .last_kind      = T_LAST_TREE_KIND,
   .gc_roots       = { T_ARCH, T_ENTITY, T_PACKAGE, T_ELAB, T_PACK_BODY,
                       T_CONTEXT },
   .gc_num_roots   = 5
};

static bool tree_kind_in(tree_t t, const tree_kind_t *list, size_t len)
{
   for (size_t i = 0; i < len; i++) {
      if (t->object.kind == list[i])
         return true;
   }

   return false;
}

static void tree_assert_kind(tree_t t, const tree_kind_t *list, size_t len,
                             const char *what)
{
   LCOV_EXCL_START
   if (unlikely(!tree_kind_in(t, list, len)))
      fatal_trace("tree kind %s is not %s",
                  tree_kind_str(t->object.kind), what);
   LCOV_EXCL_STOP
}

static void tree_assert_stmt(tree_t t)
{
   tree_assert_kind(t, stmt_kinds, ARRAY_LEN(stmt_kinds), "a statement");
}

static void tree_assert_expr(tree_t t)
{
   tree_assert_kind(t, expr_kinds, ARRAY_LEN(expr_kinds), "an expression");
}

static void tree_assert_decl(tree_t t)
{
   tree_assert_kind(t, decl_kinds, ARRAY_LEN(decl_kinds), "a declaration");
}

tree_t tree_new(tree_kind_t kind)
{
   return (tree_t)object_new(&tree_object, kind);
}

void tree_gc(void)
{
   object_gc();
}

const loc_t *tree_loc(tree_t t)
{
   assert(t != NULL);

   return &t->object.loc;
}

void tree_set_loc(tree_t t, const loc_t *loc)
{
   assert(t != NULL);
   assert(loc != NULL);

   t->object.loc = *loc;
}

ident_t tree_ident(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_IDENT);
   assert(item->ident != NULL);
   return item->ident;
}

bool tree_has_ident(tree_t t)
{
   return lookup_item(&tree_object, t, I_IDENT)->ident != NULL;
}

void tree_set_ident(tree_t t, ident_t i)
{
   lookup_item(&tree_object, t, I_IDENT)->ident = i;
}

ident_t tree_ident2(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_IDENT2);
   assert(item->ident != NULL);
   return item->ident;
}

void tree_set_ident2(tree_t t, ident_t i)
{
   lookup_item(&tree_object, t, I_IDENT2)->ident = i;
}

bool tree_has_ident2(tree_t t)
{
   return lookup_item(&tree_object, t, I_IDENT2)->ident != NULL;
}

tree_kind_t tree_kind(tree_t t)
{
   assert(t != NULL);
   return t->object.kind;
}

void tree_change_kind(tree_t t, tree_kind_t kind)
{
   object_change_kind(&tree_object, &(t->object), kind);
}

unsigned tree_ports(tree_t t)
{
   return lookup_item(&tree_object, t, I_PORTS)->tree_array.count;
}

tree_t tree_port(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_PORTS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_port(tree_t t, tree_t d)
{
   tree_assert_decl(d);
   tree_array_add(&(lookup_item(&tree_object, t, I_PORTS)->tree_array), d);
}

unsigned tree_subkind(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_SUBKIND);
   return item->ival;
}

void tree_set_subkind(tree_t t, unsigned sub)
{
   lookup_item(&tree_object, t, I_SUBKIND)->ival = sub;
}

unsigned tree_generics(tree_t t)
{
   return lookup_item(&tree_object, t, I_GENERICS)->tree_array.count;
}

tree_t tree_generic(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_GENERICS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_generic(tree_t t, tree_t d)
{
   tree_assert_decl(d);
   tree_array_add(&(lookup_item(&tree_object, t, I_GENERICS)->tree_array), d);
}

type_t tree_type(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_TYPE);
   assert(item->type != NULL);
   return item->type;
}

void tree_set_type(tree_t t, type_t ty)
{
   lookup_item(&tree_object, t, I_TYPE)->type = ty;
}

bool tree_has_type(tree_t t)
{
   return lookup_item(&tree_object, t, I_TYPE)->type != NULL;
}

unsigned tree_params(tree_t t)
{
   return lookup_item(&tree_object, t, I_PARAMS)->tree_array.count;
}

tree_t tree_param(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_PARAMS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_param(tree_t t, tree_t e)
{
   assert(tree_kind(e) == T_PARAM);
   tree_assert_expr(tree_value(e));

   tree_array_t *array = &(lookup_item(&tree_object, t, I_PARAMS)->tree_array);

   if (tree_subkind(e) == P_POS)
      tree_set_pos(e, array->count);

   tree_array_add(array, e);
}

unsigned tree_genmaps(tree_t t)
{
   return lookup_item(&tree_object, t, I_GENMAPS)->tree_array.count;
}

tree_t tree_genmap(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_GENMAPS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_genmap(tree_t t, tree_t e)
{
   tree_assert_expr(tree_value(e));

   tree_array_t *array = &(lookup_item(&tree_object, t, I_GENMAPS)->tree_array);

   if (tree_subkind(e) == P_POS)
      tree_set_pos(e, array->count);

   tree_array_add(&(lookup_item(&tree_object, t, I_GENMAPS)->tree_array), e);
}

int64_t tree_ival(tree_t t)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_INT));
   return lookup_item(&tree_object, t, I_IVAL)->ival;
}

void tree_set_ival(tree_t t, int64_t i)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_INT));
   lookup_item(&tree_object, t, I_IVAL)->ival = i;
}

double tree_dval(tree_t t)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_REAL));
   return lookup_item(&tree_object, t, I_DVAL)->dval;
}

void tree_set_dval(tree_t t, double d)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_REAL));
   lookup_item(&tree_object, t, I_DVAL)->dval = d;
}

tree_flags_t tree_flags(tree_t t)
{
   return lookup_item(&tree_object, t, I_FLAGS)->ival;
}

void tree_set_flag(tree_t t, tree_flags_t mask)
{
   lookup_item(&tree_object, t, I_FLAGS)->ival |= mask;
}

unsigned tree_chars(tree_t t)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_STRING));
   return lookup_item(&tree_object, t, I_CHARS)->ident_array.count;
}

tree_t tree_char(tree_t t, unsigned n)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_STRING));
   item_t *item = lookup_item(&tree_object, t, I_CHARS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_char(tree_t t, tree_t ref)
{
   assert((t->object.kind == T_LITERAL) && (tree_subkind(t) == L_STRING));
   tree_array_add(&(lookup_item(&tree_object, t, I_CHARS)->tree_array), ref);
}

bool tree_has_value(tree_t t)
{
   return lookup_item(&tree_object, t, I_VALUE)->tree != NULL;
}

tree_t tree_value(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_VALUE);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_value(tree_t t, tree_t v)
{
   if ((v != NULL) && (t->object.kind != T_ASSOC) && (t->object.kind != T_SPEC))
      tree_assert_expr(v);
   lookup_item(&tree_object, t, I_VALUE)->tree = v;
}

unsigned tree_decls(tree_t t)
{
   return lookup_item(&tree_object, t, I_DECLS)->tree_array.count;
}

tree_t tree_decl(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_DECLS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_decl(tree_t t, tree_t d)
{
   tree_assert_decl(d);
   tree_array_add(&(lookup_item(&tree_object, t, I_DECLS)->tree_array), d);
}

unsigned tree_stmts(tree_t t)
{
   return lookup_item(&tree_object, t, I_STMTS)->tree_array.count;
}

tree_t tree_stmt(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_STMTS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_stmt(tree_t t, tree_t s)
{
   tree_assert_stmt(s);
   tree_array_add(&(lookup_item(&tree_object, t, I_STMTS)->tree_array), s);
}

unsigned tree_waveforms(tree_t t)
{
   return lookup_item(&tree_object, t, I_WAVES)->tree_array.count;
}

tree_t tree_waveform(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_WAVES);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_waveform(tree_t t, tree_t w)
{
   assert(w->object.kind == T_WAVEFORM);
   tree_array_add(&(lookup_item(&tree_object, t, I_WAVES)->tree_array), w);
}

unsigned tree_else_stmts(tree_t t)
{
   return lookup_item(&tree_object, t, I_ELSES)->tree_array.count;
}

tree_t tree_else_stmt(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_ELSES);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_else_stmt(tree_t t, tree_t s)
{
   tree_assert_stmt(s);
   tree_array_add(&(lookup_item(&tree_object, t, I_ELSES)->tree_array), s);
}

unsigned tree_conds(tree_t t)
{
   return lookup_item(&tree_object, t, I_CONDS)->tree_array.count;
}

tree_t tree_cond(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_CONDS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_cond(tree_t t, tree_t c)
{
   assert(c->object.kind == T_COND);
   tree_array_add(&(lookup_item(&tree_object, t, I_CONDS)->tree_array), c);
}

bool tree_has_delay(tree_t t)
{
   return lookup_item(&tree_object, t, I_DELAY)->tree != NULL;
}

tree_t tree_delay(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_DELAY);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_delay(tree_t t, tree_t d)
{
   tree_assert_expr(d);
   lookup_item(&tree_object, t, I_DELAY)->tree = d;
}

unsigned tree_triggers(tree_t t)
{
   return lookup_item(&tree_object, t, I_TRIGGERS)->tree_array.count;
}

tree_t tree_trigger(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_TRIGGERS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_trigger(tree_t t, tree_t s)
{
   tree_assert_expr(s);
   tree_array_add(&(lookup_item(&tree_object, t, I_TRIGGERS)->tree_array), s);
}

unsigned tree_ops(tree_t t)
{
   return lookup_item(&tree_object, t, I_OPS)->tree_array.count;
}

tree_t tree_op(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(&tree_object, t, I_OPS)->tree_array), n);
}

void tree_add_op(tree_t t, tree_t s)
{
   assert((s->object.kind == T_FUNC_DECL) || (s->object.kind == T_PROC_DECL));
   tree_array_add(&(lookup_item(&tree_object, t, I_OPS)->tree_array), s);
}

tree_t tree_target(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_TARGET);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_target(tree_t t, tree_t lhs)
{
   lookup_item(&tree_object, t, I_TARGET)->tree = lhs;
}

tree_t tree_ref(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_REF);
   assert(item->tree != NULL);
   return item->tree;
}

bool tree_has_ref(tree_t t)
{
   return lookup_item(&tree_object, t, I_REF)->tree != NULL;
}

void tree_set_ref(tree_t t, tree_t decl)
{
   lookup_item(&tree_object, t, I_REF)->tree = decl;
}

tree_t tree_spec(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_SPEC);
   assert(item->tree != NULL);
   return item->tree;
}

bool tree_has_spec(tree_t t)
{
   return lookup_item(&tree_object, t, I_SPEC)->tree != NULL;
}

void tree_set_spec(tree_t t, tree_t s)
{
   lookup_item(&tree_object, t, I_SPEC)->tree = s;
}

unsigned tree_contexts(tree_t t)
{
   return lookup_item(&tree_object, t, I_CONTEXT)->tree_array.count;
}

tree_t tree_context(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_CONTEXT);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_context(tree_t t, tree_t ctx)
{
   assert(ctx->object.kind == T_USE || ctx->object.kind == T_LIBRARY
          || ctx->object.kind == T_CTXREF || ctx->object.kind == T_PRAGMA);
   tree_array_add(&(lookup_item(&tree_object, t, I_CONTEXT)->tree_array), ctx);
}

unsigned tree_assocs(tree_t t)
{
   return lookup_item(&tree_object, t, I_ASSOCS)->tree_array.count;
}

tree_t tree_assoc(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_ASSOCS);
   return tree_array_nth(&(item->tree_array), n);
}

void tree_add_assoc(tree_t t, tree_t a)
{
   assert(a->object.kind == T_ASSOC);

   tree_array_t *array = &(lookup_item(&tree_object, t, I_ASSOCS)->tree_array);

   if (tree_subkind(a) == A_POS)
      tree_set_pos(a, array->count);

   tree_array_add(array, a);
}

unsigned tree_nets(tree_t t)
{
   return lookup_item(&tree_object, t, I_NETS)->netid_array.count;
}

netid_t tree_net(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_NETS);
   netid_t nid = netid_array_nth(&(item->netid_array), n);
   assert(nid != NETID_INVALID);
   return nid;
}

void tree_add_net(tree_t t, netid_t n)
{
   netid_array_add(&(lookup_item(&tree_object, t, I_NETS)->netid_array), n);
}

void tree_change_net(tree_t t, unsigned n, netid_t i)
{
   item_t *item = lookup_item(&tree_object, t, I_NETS);

   if (n >= item->netid_array.count)
      netid_array_resize(&(item->netid_array), n + 1, 0xff);

   item->netid_array.items[n] = i;
}

tree_t tree_severity(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_SEVERITY);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_severity(tree_t t, tree_t s)
{
   tree_assert_expr(s);
   lookup_item(&tree_object, t, I_SEVERITY)->tree = s;
}

tree_t tree_message(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_MESSAGE);
   assert(item->tree != NULL);
   return item->tree;
}

bool tree_has_message(tree_t t)
{
   return lookup_item(&tree_object, t, I_MESSAGE)->tree != NULL;
}

void tree_set_message(tree_t t, tree_t m)
{
   tree_assert_expr(m);
   lookup_item(&tree_object, t, I_MESSAGE)->tree = m;
}

void tree_add_range(tree_t t, range_t r)
{
   range_array_add(&(lookup_item(&tree_object, t, I_RANGES)->range_array), r);
}

range_t tree_range(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_RANGES);
   return range_array_nth(&(item->range_array), n);
}

unsigned tree_ranges(tree_t t)
{
   return lookup_item(&tree_object, t, I_RANGES)->range_array.count;
}

void tree_change_range(tree_t t, unsigned n, range_t r)
{
   item_t *item = lookup_item(&tree_object, t, I_RANGES);
   assert(n < item->range_array.count);
   item->range_array.items[n] = r;
}

char *tree_text(tree_t t)
{
   return lookup_item(&tree_object, t, I_TEXT)->text;
}

void tree_set_text(tree_t t, const char *text)
{
   lookup_item(&tree_object, t, I_TEXT)->text = xstrdup(text);
}

unsigned tree_pos(tree_t t)
{
   return lookup_item(&tree_object, t, I_POS)->ival;
}

void tree_set_pos(tree_t t, unsigned pos)
{
   lookup_item(&tree_object, t, I_POS)->ival = pos;
}

class_t tree_class(tree_t t)
{
   return lookup_item(&tree_object, t, I_CLASS)->ival;
}

void tree_set_class(tree_t t, class_t c)
{
   lookup_item(&tree_object, t, I_CLASS)->ival = c;
}

tree_t tree_reject(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_REJECT);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_reject(tree_t t, tree_t r)
{
   tree_assert_expr(r);
   lookup_item(&tree_object, t, I_REJECT)->tree = r;
}

bool tree_has_reject(tree_t t)
{
   return lookup_item(&tree_object, t, I_REJECT)->tree != NULL;
}

tree_t tree_name(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_NAME);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_name(tree_t t, tree_t n)
{
   tree_assert_expr(n);
   lookup_item(&tree_object, t, I_NAME)->tree = n;
}

tree_t tree_file_mode(tree_t t)
{
   return lookup_item(&tree_object, t, I_FILE_MODE)->tree;
}

void tree_set_file_mode(tree_t t, tree_t m)
{
   lookup_item(&tree_object, t, I_FILE_MODE)->tree = m;
}

unsigned tree_visit(tree_t t, tree_visit_fn_t fn, void *context)
{
   assert(t != NULL);

   object_visit_ctx_t ctx = {
      .count      = 0,
      .postorder  = fn,
      .preorder   = NULL,
      .context    = context,
      .kind       = T_LAST_TREE_KIND,
      .generation = object_next_generation(),
      .deep       = false
   };

   object_visit(&(t->object), &ctx);

   return ctx.count;
}

unsigned tree_visit_only(tree_t t, tree_visit_fn_t fn,
                         void *context, tree_kind_t kind)
{
   assert(t != NULL);

   object_visit_ctx_t ctx = {
      .count      = 0,
      .postorder  = fn,
      .preorder   = NULL,
      .context    = context,
      .kind       = kind,
      .generation = object_next_generation(),
      .deep       = false
   };

   object_visit(&(t->object), &ctx);

   return ctx.count;
}

tree_wr_ctx_t tree_write_begin(fbuf_t *f)
{
   return (tree_wr_ctx_t)object_write_begin(f);
}

void tree_write_end(tree_wr_ctx_t ctx)
{
   object_write_end((object_wr_ctx_t *)ctx);
}

void tree_write(tree_t t, tree_wr_ctx_t ctx)
{
   object_write(&(t->object), (object_wr_ctx_t *)ctx);
}

tree_t tree_read(tree_rd_ctx_t ctx)
{
   return (tree_t)object_read((object_rd_ctx_t *)ctx, OBJECT_TAG_TREE);
}

tree_rd_ctx_t tree_read_begin(fbuf_t *f, const char *fname)
{
   return (tree_rd_ctx_t)object_read_begin(f, fname);
}

void tree_read_end(tree_rd_ctx_t ctx)
{
   object_read_end((object_rd_ctx_t *)ctx);
}

static attr_t *tree_find_attr(tree_t t, ident_t name, attr_kind_t kind)
{
   assert(t != NULL);
   assert(name != NULL);

   item_t *item = lookup_item(&tree_object, t, I_ATTRS);
   for (unsigned i = 0; i < item->attrs.num; i++) {
      if ((item->attrs.table[i].kind == kind)
          && (item->attrs.table[i].name == name))
         return &(item->attrs.table[i]);
   }

   return NULL;
}

static attr_t *tree_add_attr(tree_t t, ident_t name, attr_kind_t kind)
{
   assert(t != NULL);
   assert(name != NULL);

   attr_t *a = tree_find_attr(t, name, kind);
   if (a != NULL)
      return a;

   item_t *item = lookup_item(&tree_object, t, I_ATTRS);

   if (item->attrs.table == NULL) {
      item->attrs.alloc = 8;
      item->attrs.table = xmalloc(sizeof(attr_t) * item->attrs.alloc);
   }
   else if (item->attrs.alloc == item->attrs.num) {
      item->attrs.alloc *= 2;
      item->attrs.table = xrealloc(item->attrs.table,
                                   sizeof(attr_t) * item->attrs.alloc);
   }

   unsigned i = item->attrs.num++;
   item->attrs.table[i].kind = kind;
   item->attrs.table[i].name = name;

   return &(item->attrs.table[i]);
}

void tree_remove_attr(tree_t t, ident_t name)
{
   assert(t != NULL);
   assert(name != NULL);

   item_t *item = lookup_item(&tree_object, t, I_ATTRS);

   unsigned i;
   for (i = 0; (i < item->attrs.num)
           && (item->attrs.table[i].name != name); i++)
      ;

   if (i == item->attrs.num)
      return;

   for (; i + 1 < item->attrs.num; i++)
      item->attrs.table[i] = item->attrs.table[i + 1];

   item->attrs.num--;
}

void tree_add_attr_str(tree_t t, ident_t name, ident_t str)
{
   tree_add_attr(t, name, A_STRING)->sval = str;
}

ident_t tree_attr_str(tree_t t, ident_t name)
{
   attr_t *a = tree_find_attr(t, name, A_STRING);
   return a ? a->sval : NULL;
}

void tree_add_attr_int(tree_t t, ident_t name, int n)
{
   tree_add_attr(t, name, A_INT)->ival = n;
}

int tree_attr_int(tree_t t, ident_t name, int def)
{
   attr_t *a = tree_find_attr(t, name, A_INT);
   return a ? a->ival : def;
}

void tree_add_attr_ptr(tree_t t, ident_t name, void *ptr)
{
   tree_add_attr(t, name, A_PTR)->pval = ptr;
}

void *tree_attr_ptr(tree_t t, ident_t name)
{
   attr_t *a = tree_find_attr(t, name, A_PTR);
   return a ? a->pval : NULL;
}

tree_t tree_attr_tree(tree_t t, ident_t name)
{
   attr_t *a = tree_find_attr(t, name, A_TREE);
   return a ? a->tval : NULL;
}

void tree_add_attr_tree(tree_t t, ident_t name, tree_t val)
{
   assert(val != NULL);
   tree_add_attr(t, name, A_TREE)->tval = val;
}

tree_t tree_rewrite(tree_t t, tree_rewrite_fn_t fn, void *context)
{
   object_rewrite_ctx_t ctx = {
      .index      = 0,
      .generation = object_next_generation(),
      .fn         = fn,
      .context    = context
   };

   tree_t result = (tree_t)object_rewrite(&(t->object), &ctx);
   free(ctx.cache);
   return result;
}

tree_t tree_copy(tree_t t, tree_copy_fn_t fn, void *context)
{
   object_copy_ctx_t ctx = {
      .generation = object_next_generation(),
      .index      = 0,
      .callback   = fn,
      .context    = context,
      .copied     = NULL
   };

   object_copy_mark(&(t->object), &ctx);

   if (t->object.index == UINT32_MAX)
      return t;   // Nothing to copy

   ctx.copied = xcalloc(sizeof(void *) * ctx.index);

   tree_t copy = (tree_t)object_copy_sweep(&(t->object), &ctx);

   free(ctx.copied);
   return copy;
}

const char *tree_kind_str(tree_kind_t t)
{
   return kind_text_map[t];
}
