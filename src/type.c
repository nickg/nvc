#include "type.h"
#include "tree.h"
#include "util.h"

#include <assert.h>
#include <limits.h>

#define MAX_DIMS   4

struct type {
   type_kind_t kind;
   ident_t     ident;
   range_t     dims[MAX_DIMS];
   unsigned    n_dims;
};

#define IS(t, k) ((t)->kind == (k))
#define HAS_DIMS(t) (IS(t, T_INTEGER))

type_t type_new(type_kind_t kind)
{
   struct type *t = xmalloc(sizeof(struct type));
   t->kind   = kind;
   t->ident  = NULL;
   t->n_dims = 0;
   
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

   return t->ident;
}

void type_set_ident(type_t t, ident_t id)
{
   assert(t != NULL);
   assert(t->kind == T_UNRESOLVED);

   t->ident = id;
}

unsigned type_dims(type_t t)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));

   return t->n_dims;
}

range_t type_dim(type_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));
   assert(n < t->n_dims);

   return t->dims[n];
}

void type_add_dim(type_t t, range_t r)
{
   assert(t != NULL);
   assert(HAS_DIMS(t));
   assert(t->n_dims < MAX_DIMS);

   t->dims[t->n_dims++] = r;
}

type_t type_universal_int(void)
{
   static type_t t = NULL;

   if (t == NULL) {
      t = type_new(T_INTEGER);

      tree_t left = tree_new(T_LITERAL);
      literal_t l_min = { .kind = L_INT, .u.i = INT_MIN };
      tree_set_literal(left, l_min);

      tree_t right = tree_new(T_LITERAL);      
      literal_t l_max = { .kind = L_INT, .u.i = INT_MAX };
      tree_set_literal(right, l_max);
      
      range_t r = { .kind  = RANGE_TO,
                    .left  = left,
                    .right = right };
      type_add_dim(t, r);
   }

   return t;
}
