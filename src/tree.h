#ifndef _TREE_H
#define _TREE_H

#include "lib.h"
#include "ident.h"
#include "type.h"

typedef enum port_mode {
   PORT_INVALID,
   PORT_IN,
   PORT_OUT,
   PORT_INOUT,
   PORT_BUFFER
} port_mode_t;

typedef enum tree_kind {
   T_ENTITY,
   T_ARCH,
   T_PORT_DECL,
   T_FCALL,
   T_LITERAL,
   T_SIGNAL_DECL,
   T_PROCESS
} tree_kind_t;

typedef struct literal {
   union {
      int   i;
      char  c;
      float f;
   } u;
   enum { L_INT, L_CHAR, L_FLOAT } kind;
} literal_t;

typedef struct tree *tree_t;

tree_t tree_new(tree_kind_t kind);
tree_kind_t tree_kind(tree_t t);

// T_PORT_DECL, T_SIGNAL_DECL
type_t tree_type(tree_t t);
void tree_set_type(tree_t t, type_t ty);

// T_ENTITY, T_PORT_DECL, T_FCALL, T_ARCH, T_SIGNAL_DECL, T_PROCESS
ident_t tree_ident(tree_t t);
void tree_set_ident(tree_t t, ident_t i);

// T_ARCH
ident_t tree_ident2(tree_t t);
void tree_set_ident2(tree_t t, ident_t i);

// T_ENTITY
unsigned tree_ports(tree_t t);
tree_t tree_port(tree_t t, unsigned n);
void tree_add_port(tree_t t, tree_t d);

// T_PORT_DECL
port_mode_t tree_port_mode(tree_t t);
void tree_set_port_mode(tree_t t, port_mode_t mode);

// T_ENTITY
unsigned tree_generics(tree_t t);
tree_t tree_generic(tree_t t, unsigned n);
void tree_add_generic(tree_t t, tree_t d);

// T_FCALL
unsigned tree_params(tree_t t);
tree_t tree_param(tree_t t, unsigned n);
// TODO: what about named association? Add an ident parameter.
void tree_add_param(tree_t t, tree_t e);

// T_LITERAL
literal_t tree_literal(tree_t t);
void tree_set_literal(tree_t t, literal_t lit);

// T_PORT_DECL, T_SIGNAL_DECL
bool tree_has_value(tree_t t);
tree_t tree_value(tree_t t);
void tree_set_value(tree_t t, tree_t v);

// T_ARCH
unsigned tree_decls(tree_t t);
tree_t tree_decl(tree_t t, unsigned n);
void tree_add_decl(tree_t t, tree_t d);

// T_ARCH, T_PROCESS
unsigned tree_stmts(tree_t t);
tree_t tree_stmt(tree_t t, unsigned n);
void tree_add_stmt(tree_t t, tree_t d);

void tree_freeze(void);
void tree_store(lib_t lib, tree_t tree);
tree_t tree_load(lib_t lib, ident_t ident);

#endif  // _TREE_H
