#ifndef _TREE_H
#define _TREE_H

#include "lib.h"

typedef enum tree_kind {
   T_ENTITY
} tree_kind_t;

typedef struct tree *tree_t;

tree_t make_tree(tree_kind_t kind);

void tree_freeze(void);
void tree_store(lib_t lib, tree_t tree);
tree_t tree_load(lib_t lib, ident_t ident);

#endif  // _TREE_H
