#include "type.h"
#include "tree.h"
#include "util.h"

#include <assert.h>

struct type {
   type_kind_t kind;
   union {
      ident_t     ident;
   } u;
};

type_t type_new(type_kind_t kind)
{
   struct type *t = xmalloc(sizeof(struct type));
   t->kind = kind;

   return t;
}

type_kind_t type_kind(type_t t)
{
   assert(t != NULL);

   return t->kind;
}

bool type_eq(type_t a, type_t b)
{
   // TODO
   return false;
}

ident_t type_ident(type_t t)
{
   assert(t != NULL);
   assert(t->kind == T_UNRESOLVED);

   return t->u.ident;
}

void type_set_ident(type_t t, ident_t id)
{
   assert(t != NULL);
   assert(t->kind == T_UNRESOLVED);

   t->u.ident = id;
}

