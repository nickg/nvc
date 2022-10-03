//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "object.h"
#include "common.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static const imask_t has_map[T_LAST_TREE_KIND] = {
   // T_ENTITY
   (I_IDENT | I_PORTS | I_GENERICS | I_CONTEXT | I_DECLS | I_STMTS),

   // T_ARCH
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_CONTEXT | I_PRIMARY),

   // T_PORT_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_SUBKIND | I_CLASS | I_FLAGS),

   // T_FCALL
   (I_IDENT | I_PARAMS | I_TYPE | I_REF | I_FLAGS),

   // T_LITERAL
   (I_SUBKIND | I_TYPE | I_IVAL | I_DVAL | I_IDENT | I_REF),

   // T_SIGNAL_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FLAGS),

   // T_VAR_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FLAGS),

   // T_PROCESS
   (I_IDENT | I_DECLS | I_STMTS | I_TRIGGERS | I_FLAGS),

   // T_REF
   (I_IDENT | I_TYPE | I_REF | I_FLAGS),

   // T_WAIT
   (I_IDENT | I_VALUE | I_DELAY | I_TRIGGERS | I_FLAGS),

   // T_TYPE_DECL
   (I_IDENT | I_TYPE),

   // T_VAR_ASSIGN
   (I_IDENT | I_VALUE | I_TARGET),

   // T_PACKAGE
   (I_IDENT | I_DECLS | I_CONTEXT | I_GENERICS | I_GENMAPS),

   // T_SIGNAL_ASSIGN
   (I_IDENT | I_TARGET | I_WAVES | I_REJECT),

   // T_QUALIFIED
   (I_IDENT | I_VALUE | I_TYPE),

   // T_ENUM_LIT
   (I_IDENT | I_TYPE | I_POS),

   // T_CONST_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FLAGS),

   // T_FUNC_DECL
   (I_IDENT | I_PORTS | I_TYPE | I_FLAGS | I_IDENT2 | I_SUBKIND | I_GENERICS),

   // T_ELAB
   (I_IDENT | I_DECLS | I_STMTS | I_CONTEXT),

   // T_AGGREGATE
   (I_TYPE | I_ASSOCS | I_FLAGS),

   // T_ASSERT
   (I_IDENT | I_VALUE | I_SEVERITY | I_MESSAGE),

   // T_ATTR_REF
   (I_NAME | I_VALUE | I_IDENT | I_PARAMS | I_TYPE | I_SUBKIND),

   // T_ARRAY_REF
   (I_VALUE | I_PARAMS | I_TYPE | I_FLAGS),

   // T_ARRAY_SLICE
   (I_VALUE | I_TYPE | I_RANGES),

   // T_INSTANCE
   (I_IDENT | I_IDENT2 | I_PARAMS | I_GENMAPS | I_REF | I_CLASS | I_SPEC),

   // T_IF
   (I_IDENT | I_CONDS),

   // T_NULL
   (I_IDENT),

   // T_PACK_BODY
   (I_IDENT | I_DECLS | I_CONTEXT | I_PRIMARY),

   // T_FUNC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE | I_FLAGS | I_GENERICS
    | I_IDENT2 | I_SUBKIND),

   // T_RETURN
   (I_IDENT | I_VALUE),

   // T_COND_SIGNAL_ASSIGN
   (I_IDENT | I_TARGET | I_CONDS),

   // T_WHILE
   (I_IDENT | I_VALUE | I_STMTS),

   // T_WAVEFORM
   (I_VALUE | I_DELAY),

   // T_ALIAS
   (I_IDENT | I_VALUE | I_TYPE | I_IDENT2),

   // T_FOR
   (I_IDENT | I_STMTS | I_RANGES | I_DECLS),

   // T_ATTR_DECL
   (I_IDENT | I_TYPE),

   // T_ATTR_SPEC
   (I_IDENT | I_VALUE | I_IDENT2 | I_CLASS | I_REF),

   // T_PROC_DECL
   (I_IDENT | I_PORTS | I_TYPE | I_FLAGS | I_IDENT2 | I_SUBKIND | I_GENERICS),

   // T_PROC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE | I_FLAGS | I_GENERICS
    | I_IDENT2 | I_SUBKIND),

   // T_EXIT
   (I_IDENT | I_VALUE | I_IDENT2),

   // T_PCALL
   (I_IDENT | I_IDENT2 | I_PARAMS | I_REF),

   // T_CASE
   (I_IDENT | I_VALUE | I_ASSOCS),

   // T_BLOCK
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_GENERICS | I_PARAMS | I_GENMAPS),

   // T_COND
   (I_IDENT | I_VALUE | I_DECLS | I_STMTS),

   // T_TYPE_CONV
   (I_VALUE | I_TYPE | I_FLAGS),

   // T_SELECT
   (I_IDENT | I_VALUE | I_ASSOCS),

   // T_COMPONENT
   (I_IDENT | I_PORTS | I_GENERICS),

   // T_IF_GENERATE
   (I_IDENT | I_CONDS),

   // T_FOR_GENERATE
   (I_IDENT | I_DECLS | I_STMTS | I_RANGES),

   // T_FILE_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FILE_MODE),

   // T_OPEN
   (I_TYPE),

   // T_FIELD_DECL
   (I_IDENT | I_TYPE | I_POS),

   // T_RECORD_REF
   (I_IDENT | I_VALUE | I_TYPE | I_REF),

   // T_ALL
   (I_VALUE | I_TYPE),

   // T_NEW
   (I_VALUE | I_TYPE),

   // T_UNIT_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_NEXT
   (I_IDENT | I_VALUE | I_IDENT2),

   // T_PARAM
   (I_VALUE | I_POS | I_SUBKIND | I_NAME),

   // T_ASSOC
   (I_VALUE | I_POS | I_NAME | I_RANGES | I_SUBKIND),

   // T_USE
   (I_IDENT | I_IDENT2 | I_REF),

   // T_HIER
   (I_IDENT | I_SUBKIND | I_IDENT2 | I_REF),

   // T_SPEC
   (I_IDENT | I_IDENT2 | I_VALUE | I_REF | I_DECLS),

   // T_BINDING
   (I_PARAMS | I_GENMAPS | I_IDENT | I_IDENT2 | I_CLASS | I_REF),

   // T_LIBRARY
   (I_IDENT | I_IDENT2),

   // T_DESIGN_UNIT
   (I_CONTEXT),

   // T_CONFIGURATION
   (I_IDENT | I_IDENT2 | I_DECLS | I_PRIMARY),

   // T_PROT_BODY
   (I_IDENT | I_TYPE | I_DECLS),

   // T_CONTEXT
   (I_CONTEXT | I_IDENT),

   // T_CONTEXT_REF
   (I_IDENT | I_REF),

   // T_CONSTRAINT
   (I_SUBKIND | I_RANGES | I_REF),

   // T_BLOCK_CONFIG
   (I_DECLS | I_IDENT | I_VALUE | I_RANGES | I_REF),

   // T_PROT_FCALL
   (I_IDENT | I_PARAMS | I_TYPE | I_REF | I_FLAGS | I_NAME),

   // T_PROT_PCALL
   (I_IDENT | I_IDENT2 | I_PARAMS | I_REF | I_NAME),

   // T_RANGE
   (I_SUBKIND | I_VALUE | I_LEFT | I_RIGHT | I_TYPE),

   // T_IMPLICIT_SIGNAL
   (I_IDENT | I_TYPE | I_SUBKIND | I_VALUE | I_FLAGS),

   // T_DISCONNECT
   (I_IDENT | I_REF | I_TYPE | I_DELAY),

   // T_GROUP_TEMPLATE
   (I_IDENT),

   // T_GROUP
   (I_IDENT | I_REF),

   // T_SUBTYPE_DECL
   (I_IDENT | I_TYPE),

   // T_COND_VAR_ASSIGN
   (I_IDENT | I_TARGET | I_CONDS),

   // T_CONV_FUNC
   (I_IDENT | I_REF | I_VALUE | I_TYPE),

   // T_CONCURRENT
   (I_IDENT | I_STMTS | I_GUARD | I_FLAGS),

   // T_SEQUENCE
   (I_IDENT | I_STMTS | I_DECLS),

   // T_PACK_INST
   (I_IDENT | I_REF | I_DECLS | I_CONTEXT | I_GENERICS | I_GENMAPS),

   // T_GENERIC_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_CLASS | I_SUBKIND | I_FLAGS | I_PORTS),

   // T_TYPE_REF
   (I_IDENT | I_TYPE),

   // T_BOX
   (I_TYPE),

   // T_PARAM_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_SUBKIND | I_CLASS | I_FLAGS),

   // T_EXTERNAL_NAME
   (I_PARTS | I_CLASS | I_TYPE | I_REF),

   // T_FORCE
   (I_IDENT | I_TARGET | I_VALUE | I_SUBKIND),

   // T_RELEASE
   (I_IDENT | I_TARGET | I_SUBKIND),

   // T_PROTECTED_REF
   (I_IDENT | I_VALUE | I_TYPE | I_REF),

   // T_MATCH_CASE
   (I_IDENT | I_VALUE | I_ASSOCS),

   // T_FUNC_INST
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE | I_FLAGS | I_GENERICS
    | I_IDENT2 | I_SUBKIND | I_GENMAPS | I_REF),

   // T_PROC_INST
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE | I_FLAGS | I_GENERICS
    | I_IDENT2 | I_SUBKIND | I_GENMAPS | I_REF),

   // T_ELEM_CONSTRAINT
   (I_IDENT | I_REF | I_TYPE),

   // T_STRING
   (I_CHARS | I_TYPE),

   // T_PATH_ELT
   (I_SUBKIND | I_IDENT | I_VALUE),
};

static const char *kind_text_map[T_LAST_TREE_KIND] = {
   "T_ENTITY",          "T_ARCH",            "T_PORT_DECL",
   "T_FCALL",           "T_LITERAL",         "T_SIGNAL_DECL",
   "T_VAR_DECL",        "T_PROCESS",         "T_REF",
   "T_WAIT",            "T_TYPE_DECL",       "T_VAR_ASSIGN",
   "T_PACKAGE",         "T_SIGNAL_ASSIGN",   "T_QUALIFIED",
   "T_ENUM_LIT",        "T_CONST_DECL",      "T_FUNC_DECL",
   "T_ELAB",            "T_AGGREGATE",       "T_ASSERT",
   "T_ATTR_REF",        "T_ARRAY_REF",       "T_ARRAY_SLICE",
   "T_INSTANCE",        "T_IF",              "T_NULL",
   "T_PACK_BODY",       "T_FUNC_BODY",       "T_RETURN",
   "T_COND_ASSIGN",     "T_WHILE",           "T_WAVEFORM",
   "T_ALIAS",           "T_FOR",             "T_ATTR_DECL",
   "T_ATTR_SPEC",       "T_PROC_DECL",       "T_PROC_BODY",
   "T_EXIT",            "T_PCALL",           "T_CASE",
   "T_BLOCK",           "T_COND",            "T_TYPE_CONV",
   "T_SELECT",          "T_COMPONENT",       "T_IF_GENERATE",
   "T_FOR_GENERATE",    "T_FILE_DECL",       "T_OPEN",
   "T_FIELD_DECL",      "T_RECORD_REF",      "T_ALL",
   "T_NEW",             "T_UNIT_DECL",       "T_NEXT",
   "T_PARAM",           "T_ASSOC",           "T_USE",
   "T_HIER",            "T_SPEC",            "T_BINDING",
   "T_LIBRARY",         "T_DESIGN_UNIT",     "T_CONFIGURATION",
   "T_PROT_BODY",       "T_CONTEXT",         "T_CONTEXT_REF",
   "T_CONSTRAINT",      "T_BLOCK_CONFIG",    "T_PROT_FCALL",
   "T_PROT_PCALL",      "T_RANGE",           "T_IMPLICIT_SIGNAL",
   "T_DISCONNECT",      "T_GROUP_TEMPLATE",  "T_GROUP",
   "T_SUBTYPE_DECL",    "T_COND_VAR_ASSIGN", "T_CONV_FUNC",
   "T_CONCURRENT",      "T_SEQUENCE",        "T_PACK_INST",
   "T_GENERIC_DECL",    "T_TYPE_REF",        "T_BOX",
   "T_PARAM_DECL",      "T_EXTERNAL_NAME",   "T_FORCE",
   "T_RELEASE",         "T_PROT_REF",        "T_MATCH_CASE",
   "T_FUNC_INST",       "T_PROC_INST",       "T_ELEM_CONSTRAINT",
   "T_STRING",          "T_PATH_ELT",
};

static const change_allowed_t change_allowed[] = {
   { T_REF,         T_FCALL         },
   { T_REF,         T_PCALL         },
   { T_ARRAY_REF,   T_FCALL         },
   { T_FCALL,       T_ARRAY_REF     },
   { T_DESIGN_UNIT, T_ENTITY        },
   { T_DESIGN_UNIT, T_PACKAGE       },
   { T_DESIGN_UNIT, T_PACK_BODY     },
   { T_DESIGN_UNIT, T_ARCH          },
   { T_DESIGN_UNIT, T_CONFIGURATION },
   { T_DESIGN_UNIT, T_CONTEXT       },
   { T_DESIGN_UNIT, T_PACK_INST     },
   { T_FUNC_DECL,   T_FUNC_BODY     },
   { T_PROC_DECL,   T_PROC_BODY     },
   { T_FCALL,       T_PROT_FCALL    },
   { T_PCALL,       T_PROT_PCALL    },
   { -1,            -1              }
};

struct _tree {
   object_t object;
};

struct _type {
   object_t object;
};

static const tree_kind_t stmt_kinds[] = {
   T_PROCESS,    T_WAIT,            T_VAR_ASSIGN,   T_SIGNAL_ASSIGN,
   T_ASSERT,     T_INSTANCE,        T_IF,           T_NULL,
   T_RETURN,     T_COND_ASSIGN,     T_WHILE,        T_FOR,
   T_EXIT,       T_PCALL,           T_CASE,         T_BLOCK,
   T_SELECT,     T_IF_GENERATE,     T_FOR_GENERATE, T_NEXT,
   T_PROT_PCALL, T_COND_VAR_ASSIGN, T_CONCURRENT,   T_FORCE,
   T_RELEASE,    T_MATCH_CASE,      T_SEQUENCE,
};

static tree_kind_t expr_kinds[] = {
   T_FCALL,     T_LITERAL,       T_REF,        T_QUALIFIED,
   T_AGGREGATE, T_ATTR_REF,      T_ARRAY_REF,  T_ARRAY_SLICE,
   T_TYPE_CONV, T_OPEN,          T_RECORD_REF, T_ALL,
   T_NEW,       T_PROT_FCALL,    T_CONV_FUNC,  T_TYPE_REF,
   T_BOX,       T_EXTERNAL_NAME, T_PROT_REF,   T_STRING,
};

static tree_kind_t decl_kinds[] = {
   T_PORT_DECL,      T_SIGNAL_DECL,    T_VAR_DECL,        T_TYPE_DECL,
   T_CONST_DECL,     T_FUNC_DECL,      T_FUNC_BODY,       T_ALIAS,
   T_ATTR_DECL,      T_ATTR_SPEC,      T_PROC_DECL,       T_PROC_BODY,
   T_COMPONENT,      T_FILE_DECL,      T_FIELD_DECL,      T_UNIT_DECL,
   T_HIER,           T_SPEC,           T_BINDING,         T_USE,
   T_PROT_BODY,      T_BLOCK_CONFIG,   T_IMPLICIT_SIGNAL, T_DISCONNECT,
   T_GROUP_TEMPLATE, T_GROUP,          T_SUBTYPE_DECL,    T_PACKAGE,
   T_PACK_BODY,      T_PACK_INST,      T_GENERIC_DECL,    T_PARAM_DECL,
   T_PROC_INST,      T_FUNC_INST,
};

object_class_t tree_object = {
   .name           = "tree",
   .change_allowed = change_allowed,
   .has_map        = has_map,
   .kind_text_map  = kind_text_map,
   .tag            = OBJECT_TAG_TREE,
   .last_kind      = T_LAST_TREE_KIND,
   .gc_roots       = { T_ARCH, T_ENTITY, T_PACKAGE, T_ELAB, T_PACK_BODY,
                       T_CONTEXT, T_CONFIGURATION, T_DESIGN_UNIT,
                       T_PACK_INST },
   .gc_num_roots   = 9
};

object_arena_t *global_arena = NULL;

static void tree_assert_kind(tree_t t, const tree_kind_t *list, size_t len,
                             const char *what)
{
#ifndef NDEBUG
   for (size_t i = 0; i < len; i++) {
      if (t->object.kind == list[i])
         return;
   }

   fatal_trace("tree kind %s is not %s", tree_kind_str(t->object.kind), what);
#endif
}

static inline void tree_assert_stmt(tree_t t)
{
   tree_assert_kind(t, stmt_kinds, ARRAY_LEN(stmt_kinds), "a statement");
}

static inline void tree_assert_expr(tree_t t)
{
   tree_assert_kind(t, expr_kinds, ARRAY_LEN(expr_kinds), "an expression");
}

static inline void tree_assert_decl(tree_t t)
{
   tree_assert_kind(t, decl_kinds, ARRAY_LEN(decl_kinds), "a declaration");
}

static inline tree_t tree_array_nth(item_t *item, unsigned n)
{
   object_t *o = obj_array_nth(item->obj_array, n);
   return container_of(o, struct _tree, object);
}

static inline void tree_array_add(item_t *item, tree_t t)
{
   obj_array_add(&(item->obj_array), &(t->object));
}

tree_t tree_new(tree_kind_t kind)
{
   return (tree_t)object_new(global_arena, &tree_object, kind);
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
   item_t *item = lookup_item(&tree_object, t, I_PORTS);
   return obj_array_count(item->obj_array);
}

tree_t tree_port(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_PORTS);
   return tree_array_nth(item, n);
}

void tree_add_port(tree_t t, tree_t d)
{
   tree_assert_decl(d);
   tree_array_add(lookup_item(&tree_object, t, I_PORTS), d);
   object_write_barrier(&(t->object), &(d->object));
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
   item_t *item = lookup_item(&tree_object, t, I_GENERICS);
   return obj_array_count(item->obj_array);
}

tree_t tree_generic(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_GENERICS);
   return tree_array_nth(item, n);
}

void tree_add_generic(tree_t t, tree_t d)
{
   assert(d->object.kind == T_GENERIC_DECL);
   tree_array_add(lookup_item(&tree_object, t, I_GENERICS), d);
   object_write_barrier(&(t->object), &(d->object));
}

type_t tree_type(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_TYPE);
   assert(item->object != NULL);
   return container_of(item->object, struct _type, object);
}

void tree_set_type(tree_t t, type_t ty)
{
   lookup_item(&tree_object, t, I_TYPE)->object = &(ty->object);
   object_write_barrier(&(t->object), &(ty->object));
}

bool tree_has_type(tree_t t)
{
   return lookup_item(&tree_object, t, I_TYPE)->object != NULL;
}

unsigned tree_params(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_PARAMS);
   return obj_array_count(item->obj_array);
}

tree_t tree_param(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_PARAMS);
   return tree_array_nth(item, n);
}

void tree_add_param(tree_t t, tree_t e)
{
   assert(e->object.kind == T_PARAM);
   tree_array_add(lookup_item(&tree_object, t, I_PARAMS), e);
   object_write_barrier(&(t->object), &(e->object));
}

unsigned tree_genmaps(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_GENMAPS);
   return obj_array_count(item->obj_array);
}

tree_t tree_genmap(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_GENMAPS);
   return tree_array_nth(item, n);
}

void tree_add_genmap(tree_t t, tree_t e)
{
   tree_array_add(lookup_item(&tree_object, t, I_GENMAPS), e);
}

void tree_trim_genmaps(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_GENMAPS);
   assert(n < obj_array_count(item->obj_array));
   assert(n > 0);
   item->obj_array->count = n;
}

int64_t tree_ival(tree_t t)
{
   return lookup_item(&tree_object, t, I_IVAL)->ival;
}

void tree_set_ival(tree_t t, int64_t i)
{
   lookup_item(&tree_object, t, I_IVAL)->ival = i;
}

double tree_dval(tree_t t)
{
   return lookup_item(&tree_object, t, I_DVAL)->dval;
}

void tree_set_dval(tree_t t, double d)
{
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

void tree_clear_flag(tree_t t, tree_flags_t mask)
{
   lookup_item(&tree_object, t, I_FLAGS)->ival &= ~mask;
}

tree_t tree_primary(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_PRIMARY);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

bool tree_has_primary(tree_t t)
{
   return lookup_item(&tree_object, t, I_PRIMARY)->object != NULL;
}

void tree_set_primary(tree_t t, tree_t unit)
{
   lookup_item(&tree_object, t, I_PRIMARY)->object = &(unit->object);
   object_write_barrier(&(t->object), &(unit->object));
}

unsigned tree_chars(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_CHARS);
   return obj_array_count(item->obj_array);
}

tree_t tree_char(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_CHARS);
   return tree_array_nth(item, n);
}

void tree_add_char(tree_t t, tree_t ref)
{
   tree_array_add(lookup_item(&tree_object, t, I_CHARS), ref);
   object_write_barrier(&(t->object), &(ref->object));
}

unsigned tree_parts(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_PARTS);
   return obj_array_count(item->obj_array);
}

tree_t tree_part(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_PARTS);
   return tree_array_nth(item, n);
}

void tree_add_part(tree_t t, tree_t ref)
{
   tree_array_add(lookup_item(&tree_object, t, I_PARTS), ref);
   object_write_barrier(&(t->object), &(ref->object));
}

bool tree_has_value(tree_t t)
{
   return lookup_item(&tree_object, t, I_VALUE)->object != NULL;
}

tree_t tree_value(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_VALUE);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_value(tree_t t, tree_t v)
{
   if ((v != NULL) && (t->object.kind != T_ASSOC) && (t->object.kind != T_SPEC))
      tree_assert_expr(v);
   lookup_item(&tree_object, t, I_VALUE)->object = &(v->object);
   object_write_barrier(&(t->object), &(v->object));
}

unsigned tree_decls(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_DECLS);
   return obj_array_count(item->obj_array);
}

tree_t tree_decl(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_DECLS);
   return tree_array_nth(item, n);
}

void tree_add_decl(tree_t t, tree_t d)
{
   tree_assert_decl(d);
   tree_array_add(lookup_item(&tree_object, t, I_DECLS), d);
   object_write_barrier(&(t->object), &(d->object));
}

unsigned tree_stmts(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_STMTS);
   return obj_array_count(item->obj_array);
}

tree_t tree_stmt(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_STMTS);
   return tree_array_nth(item, n);
}

void tree_add_stmt(tree_t t, tree_t s)
{
   tree_assert_stmt(s);
   tree_array_add(lookup_item(&tree_object, t, I_STMTS), s);
   object_write_barrier(&(t->object), &(s->object));
}

unsigned tree_waveforms(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_WAVES);
   return obj_array_count(item->obj_array);
}

tree_t tree_waveform(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_WAVES);
   return tree_array_nth(item, n);
}

void tree_add_waveform(tree_t t, tree_t w)
{
   assert(w->object.kind == T_WAVEFORM);
   tree_array_add(lookup_item(&tree_object, t, I_WAVES), w);
   object_write_barrier(&(t->object), &(w->object));
}

unsigned tree_conds(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_CONDS);
   return obj_array_count(item->obj_array);
}

tree_t tree_cond(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_CONDS);
   return tree_array_nth(item, n);
}

void tree_add_cond(tree_t t, tree_t c)
{
   assert(c->object.kind == T_COND);
   tree_array_add(lookup_item(&tree_object, t, I_CONDS), c);
   object_write_barrier(&(t->object), &(c->object));
}

bool tree_has_delay(tree_t t)
{
   return lookup_item(&tree_object, t, I_DELAY)->object != NULL;
}

tree_t tree_delay(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_DELAY);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_delay(tree_t t, tree_t d)
{
   tree_assert_expr(d);
   lookup_item(&tree_object, t, I_DELAY)->object = &(d->object);
   object_write_barrier(&(t->object), &(d->object));
}

unsigned tree_triggers(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_TRIGGERS);
   return obj_array_count(item->obj_array);
}

tree_t tree_trigger(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_TRIGGERS);
   return tree_array_nth(item, n);
}

void tree_add_trigger(tree_t t, tree_t s)
{
   tree_assert_expr(s);
   tree_array_add(lookup_item(&tree_object, t, I_TRIGGERS), s);
   object_write_barrier(&(t->object), &(s->object));
}

tree_t tree_target(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_TARGET);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_target(tree_t t, tree_t lhs)
{
   lookup_item(&tree_object, t, I_TARGET)->object = &(lhs->object);
   object_write_barrier(&(t->object), &(lhs->object));
}

tree_t tree_ref(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_REF);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

bool tree_has_ref(tree_t t)
{
   return lookup_item(&tree_object, t, I_REF)->object != NULL;
}

void tree_set_ref(tree_t t, tree_t decl)
{
   lookup_item(&tree_object, t, I_REF)->object = &(decl->object);
   object_write_barrier(&(t->object), &(decl->object));
}

tree_t tree_spec(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_SPEC);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

bool tree_has_spec(tree_t t)
{
   return lookup_item(&tree_object, t, I_SPEC)->object != NULL;
}

void tree_set_spec(tree_t t, tree_t s)
{
   lookup_item(&tree_object, t, I_SPEC)->object = &(s->object);
   object_write_barrier(&(t->object), &(s->object));
}

unsigned tree_contexts(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_CONTEXT);
   return obj_array_count(item->obj_array);
}

tree_t tree_context(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_CONTEXT);
   return tree_array_nth(item, n);
}

void tree_add_context(tree_t t, tree_t ctx)
{
   assert(ctx->object.kind == T_USE || ctx->object.kind == T_LIBRARY
          || ctx->object.kind == T_CONTEXT_REF);
   tree_array_add(lookup_item(&tree_object, t, I_CONTEXT), ctx);
   object_write_barrier(&(t->object), &(ctx->object));
}

unsigned tree_assocs(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_ASSOCS);
   return obj_array_count(item->obj_array);
}

tree_t tree_assoc(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_ASSOCS);
   return tree_array_nth(item, n);
}

void tree_add_assoc(tree_t t, tree_t a)
{
   assert(a->object.kind == T_ASSOC);
   tree_array_add(lookup_item(&tree_object, t, I_ASSOCS), a);
   object_write_barrier(&(t->object), &(a->object));
}

tree_t tree_severity(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_SEVERITY);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_severity(tree_t t, tree_t s)
{
   tree_assert_expr(s);
   lookup_item(&tree_object, t, I_SEVERITY)->object = &(s->object);
   object_write_barrier(&(t->object), &(s->object));
}

tree_t tree_message(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_MESSAGE);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

bool tree_has_message(tree_t t)
{
   return lookup_item(&tree_object, t, I_MESSAGE)->object != NULL;
}

void tree_set_message(tree_t t, tree_t m)
{
   tree_assert_expr(m);
   lookup_item(&tree_object, t, I_MESSAGE)->object = &(m->object);
   object_write_barrier(&(t->object), &(m->object));
}

void tree_add_range(tree_t t, tree_t r)
{
   tree_array_add(lookup_item(&tree_object, t, I_RANGES), r);
   object_write_barrier(&(t->object), &(r->object));
}

tree_t tree_range(tree_t t, unsigned n)
{
   item_t *item = lookup_item(&tree_object, t, I_RANGES);
   return tree_array_nth(item, n);
}

unsigned tree_ranges(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_RANGES);
   return obj_array_count(item->obj_array);
}

unsigned tree_pos(tree_t t)
{
   return lookup_item(&tree_object, t, I_POS)->ival;
}

void tree_set_pos(tree_t t, unsigned pos)
{
   lookup_item(&tree_object, t, I_POS)->ival = pos;
}

tree_t tree_left(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_LEFT);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_left(tree_t t, tree_t left)
{
   tree_assert_expr(left);
   lookup_item(&tree_object, t, I_LEFT)->object = &(left->object);
   object_write_barrier(&(t->object), &(left->object));
}

tree_t tree_right(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_RIGHT);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_right(tree_t t, tree_t right)
{
   tree_assert_expr(right);
   lookup_item(&tree_object, t, I_RIGHT)->object = &(right->object);
   object_write_barrier(&(t->object), &(right->object));
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
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_reject(tree_t t, tree_t r)
{
   tree_assert_expr(r);
   lookup_item(&tree_object, t, I_REJECT)->object = &(r->object);
   object_write_barrier(&(t->object), &(r->object));
}

bool tree_has_reject(tree_t t)
{
   return lookup_item(&tree_object, t, I_REJECT)->object != NULL;
}

tree_t tree_guard(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_GUARD);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_guard(tree_t t, tree_t g)
{
   assert(g->object.kind == T_REF);
   lookup_item(&tree_object, t, I_GUARD)->object = &(g->object);
   object_write_barrier(&(t->object), &(g->object));
}

bool tree_has_guard(tree_t t)
{
   return lookup_item(&tree_object, t, I_GUARD)->object != NULL;
}

tree_t tree_name(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_NAME);
   assert(item->object != NULL);
   return container_of(item->object, struct _tree, object);
}

void tree_set_name(tree_t t, tree_t n)
{
   tree_assert_expr(n);
   lookup_item(&tree_object, t, I_NAME)->object = &(n->object);
   object_write_barrier(&(t->object), &(n->object));
}

bool tree_has_name(tree_t t)
{
   return lookup_item(&tree_object, t, I_NAME)->object != NULL;
}

tree_t tree_file_mode(tree_t t)
{
   item_t *item = lookup_item(&tree_object, t, I_FILE_MODE);
   return container_of(item->object, struct _tree, object);
}

void tree_set_file_mode(tree_t t, tree_t m)
{
   lookup_item(&tree_object, t, I_FILE_MODE)->object = &(m->object);
   object_write_barrier(&(t->object), &(m->object));
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
      .deep       = false,
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

void tree_write(tree_t t, fbuf_t *f, ident_wr_ctx_t ident_ctx,
                loc_wr_ctx_t *loc_ctx)
{
   if (global_arena != NULL) {
      object_arena_freeze(global_arena);
      global_arena = NULL;
   }

   object_write(&(t->object), f, ident_ctx, loc_ctx);
}

tree_t tree_read(fbuf_t *f, tree_load_fn_t find_deps_fn,
                 ident_rd_ctx_t ident_ctx, loc_rd_ctx_t *loc_ctx)
{
   object_t *o = object_read(f, (object_load_fn_t)find_deps_fn,
                             ident_ctx, loc_ctx);
   assert(o->tag == OBJECT_TAG_TREE);
   return container_of(o, struct _tree, object);
}

tree_t tree_rewrite(tree_t t, tree_rewrite_pre_fn_t pre_fn,
                    tree_rewrite_post_fn_t tree_post_fn,
                    type_rewrite_post_fn_t type_post_fn,
                    void *context)
{
   assert(global_arena != NULL);

   object_rewrite_ctx_t ctx = {
      .generation = object_next_generation(),
      .context    = context,
      .arena      = global_arena,
   };

   ctx.pre_fn[OBJECT_TAG_TREE] = (object_rewrite_pre_fn_t)pre_fn;

   ctx.post_fn[OBJECT_TAG_TREE] = (object_rewrite_post_fn_t)tree_post_fn;
   ctx.post_fn[OBJECT_TAG_TYPE] = (object_rewrite_post_fn_t)type_post_fn;

   object_t *result = object_rewrite(&(t->object), &ctx);
   free(ctx.cache);
   return container_of(result, struct _tree, object);
}

void tree_copy(tree_t *roots, unsigned nroots,
               tree_copy_pred_t tree_pred,
               type_copy_pred_t type_pred,
               tree_copy_fn_t tree_callback,
               type_copy_fn_t type_callback,
               void *context)
{
   object_copy_ctx_t *ctx LOCAL = xcalloc_flex(sizeof(object_copy_ctx_t),
                                               nroots, sizeof(object_t *));

   ctx->generation = object_next_generation();
   ctx->context    = context;
   ctx->arena      = global_arena;
   ctx->nroots     = nroots;

   for (unsigned i = 0; i < nroots; i++)
      ctx->roots[i] = &(roots[i]->object);

   ctx->should_copy[OBJECT_TAG_TREE] = (object_copy_pred_t)tree_pred;
   ctx->should_copy[OBJECT_TAG_TYPE] = (object_copy_pred_t)type_pred;

   ctx->callback[OBJECT_TAG_TREE] = (object_copy_fn_t)tree_callback;
   ctx->callback[OBJECT_TAG_TYPE] = (object_copy_fn_t)type_callback;

   object_copy(ctx);

   for (unsigned i = 0; i < nroots; i++)
      roots[i] = container_of(ctx->roots[i], struct _tree, object);
}

const char *tree_kind_str(tree_kind_t t)
{
   return kind_text_map[t];
}

void freeze_global_arena(void)
{
   if (global_arena != NULL) {
      object_arena_freeze(global_arena);
      global_arena = NULL;
   }
}

void make_new_arena(void)
{
   freeze_global_arena();
   global_arena = object_arena_new(object_arena_default_size(), standard());
}

object_arena_t *tree_arena(tree_t t)
{
   return object_arena(&(t->object));
}

bool tree_frozen(tree_t t)
{
   return arena_frozen(object_arena(&(t->object)));
}

tree_t tree_container(tree_t t)
{
   object_t *o = arena_root(object_arena(&(t->object)));
   assert(o->tag == OBJECT_TAG_TREE);
   return container_of(o, struct _tree, object);
}

void tree_locus(tree_t t, ident_t *unit, ptrdiff_t *offset)
{
   assert(t != NULL);
   object_locus(&(t->object), unit, offset);
}

tree_t tree_from_locus(ident_t unit, ptrdiff_t offset,
                       tree_load_fn_t find_deps_fn)
{
   object_t *o = object_from_locus(unit, offset,
                                   (object_load_fn_t)find_deps_fn,
                                   OBJECT_TAG_TREE);
   assert(o->tag == OBJECT_TAG_TREE);
   return container_of(o, struct _tree, object);
}

void tree_walk_deps(tree_t t, tree_deps_fn_t fn, void *ctx)
{
   object_arena_walk_deps(object_arena(&(t->object)), fn, ctx);
}

int tree_stable_compar(const void *pa, const void *pb)
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
