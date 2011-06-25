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
   T_PORT_DECL
} tree_kind_t;

/**
 * TODO describe
 */
typedef struct tree *tree_t;

/**
 * Allocate a new tree node.
 *
 * \param kind Type of tree node.
 * \return Handle to tree node. Never NULL.
 */
tree_t tree_new(tree_kind_t kind);

/**
 * Return the kind of the tree.
 *
 * \param t Tree of any kind.
 * \return Kind of the tree.
 */
tree_kind_t tree_kind(tree_t t);

/**
 * Return the VHDL type associated with a tree node.
 *
 * \param t One of T_PORT_DECL.
 */
type_t tree_type(tree_t t);

/**
 * Change the VHDL type of a tree node.
 *
 * \param t One of T_PORT_DECL.
 */
void tree_set_type(tree_t t, type_t ty);

/**
 * Get the identifier associated with the tree.
 *
 * Will abort if no identifier has been set.
 *
 * \param t One of T_ENTITY, T_PORT_DECL.
 * \return Identifier of tree.
 */
ident_t tree_ident(tree_t t);

/**
 * Set the identifier of a tree.
 *
 * \param t One of T_ENTITY, T_PORT_DECL.
 * \param i New identifier.
 */
void tree_set_ident(tree_t t, ident_t i);

/**
 * Count the number of ports in an entity or component or the number
 * of parameters to a function.
 *
 * \param t One of T_ENTITY.
 * \return Number of ports.
 */
unsigned tree_ports(tree_t t);

/**
 * Get the Nth entity port or function parameter.
 *
 * \param n Number of port to get.
 * \return Nth port declaration.
 */
tree_t tree_port(tree_t t, unsigned n);

/**
 * Add a declaration to the ports or arguments list.
 *
 * \param t One of T_ENTITY.
 * \param d One of T_PORT_DECL.
 */
void tree_add_port(tree_t t, tree_t d);

/**
 * The direction of the port or parameter.
 *
 * \param t One of T_PORT_DECL.
 * \return Direction of port.
 */
port_mode_t tree_port_mode(tree_t t);

/**
 * Set the direction of a port or parameter declaration.
 *
 * \param t One of T_PORT_DECL.
 */
void tree_set_port_mode(tree_t t, port_mode_t mode);

/**
 * Count the number of generics in a entity or component.
 *
 * \param t One of T_ENTITY.
 * \return Number of generics.
 */
unsigned tree_generics(tree_t t);

/**
 * Get the Nth entity generic.
 *
 * \param n Number of generic to get.
 * \return Nth generic declaration.
 */
tree_t tree_generic(tree_t t, unsigned n);

/**
 * Add a declaration to the generics list.
 *
 * \param t One of T_ENTITY.
 * \param d One of T_PORT_DECL.
 */
void tree_add_generic(tree_t t, tree_t d);

void tree_freeze(void);
void tree_store(lib_t lib, tree_t tree);
tree_t tree_load(lib_t lib, ident_t ident);

#endif  // _TREE_H
