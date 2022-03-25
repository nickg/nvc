//
//  Copyright (C) 2011-2018  Nick Gasson
//  Copyright (C) 2019       Sebastien Van Cauwenberghe
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

#include "phase.h"
#include "util.h"
#include "common.h"
#include "json.h"
#include "diag.h"
#include "type.h"

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdlib.h>

static JsonNode *dump_expr(tree_t t);
static JsonNode *dump_stmt(tree_t t);
static JsonNode *dump_port(tree_t t);
static JsonNode *dump_decl(tree_t t);
static JsonNode *dump_decls(tree_t t);

typedef tree_t (*get_fn_t)(tree_t, unsigned);

static void cannot_dump(tree_t t, const char *hint)
{
   printf("\n");
   fflush(stdout);
   fatal("cannot dump %s kind %s", hint, tree_kind_str(tree_kind(t)));
}

static void add_lineno(JsonNode *obj, tree_t node)
{
   const loc_t *loc = tree_loc(node);
   json_append_member(obj, "ln", json_mknumber(loc->first_line));
}

static void add_filename(JsonNode *obj, tree_t node)
{
   const loc_t *loc = tree_loc(node);
   json_append_member(obj, "filename", json_mkstring(loc_file_str(loc)));
}

static JsonNode *dump_params(tree_t t, get_fn_t get, int n, const char *prefix)
{
   JsonNode *params = json_mkarray();
   if (n > 0) {
      for (int i = 0; i < n; i++) {
         JsonNode *element = json_mkobject();
         json_append_element(params, element);
         tree_t p = (*get)(t, i);
         add_lineno(element, t);
         switch (tree_subkind(p)) {
         case P_POS:
            json_append_member(element, "name", json_mknull());
            break;
         case P_NAMED:
            json_append_member(element, "name", dump_expr(tree_name(p)));
            break;
         }
         json_append_member(element, "value", dump_expr(tree_value(p)));
      }
   }
   return params;
}

static JsonNode *dump_range(tree_t r)
{
   JsonNode *range_obj = json_mkobject();
   json_append_member(range_obj, "cls", json_mkstring("range"));
   add_lineno(range_obj, r);
   switch (tree_subkind(r)) {
   case RANGE_TO:
   case RANGE_DOWNTO:
      json_append_member(range_obj, "l", dump_expr(tree_left(r)));
      json_append_member(range_obj, "r", dump_expr(tree_right(r)));
      json_append_member(range_obj, "dir",
                         json_mkstring(tree_subkind(r) == RANGE_TO
                                       ? "to" : "downto"));
      break;
   case RANGE_EXPR:
      json_append_member(range_obj, "expr", dump_expr(tree_value(r)));
      break;
   case RANGE_ERROR:
      break;
   }
   return range_obj;
}

static JsonNode *dump_expr(tree_t t) //TODO: incomplete
{
   JsonNode *expr_node = json_mkobject();
   add_lineno(expr_node, t);
   switch (tree_kind(t)) {
   case T_FCALL:
      json_append_member(expr_node, "cls", json_mkstring("fcall"));
      json_append_member(expr_node, "name", json_mkstring(istr(tree_ident(tree_ref(t)))));
      json_append_member(expr_node, "params", dump_params(t, tree_param, tree_params(t), NULL));
      break;

   case T_LITERAL:
      switch (tree_subkind(t)) {
      case L_INT:
         json_append_member(expr_node, "cls", json_mkstring("int"));
         json_append_member(expr_node, "value", json_mknumber(tree_ival(t)));
         break;
      case L_REAL:
         json_append_member(expr_node, "cls", json_mkstring("real"));
         json_append_member(expr_node, "value", json_mknumber(tree_dval(t)));
         break;
      case L_NULL:
         json_append_member(expr_node, "cls", json_mkstring("null"));
         json_append_member(expr_node, "value", json_mknull());
         break;
      case L_STRING:
         if (type_ident(tree_type(t)) == ident_new("STD.STANDARD.STRING"))
            json_append_member(expr_node, "cls", json_mkstring("string"));
         else
            json_append_member(expr_node, "cls", json_mkstring("string_lit"));
         {
            const int nchars = tree_chars(t);
            char *str = malloc(nchars+1);
               for (int i = 0; i < nchars; i++)
                  str[i] = ident_char(tree_ident(tree_char(t, i)), 1);
            str[nchars] = '\0';
            json_append_member(expr_node, "val", json_mkstring(str));
            free(str);
         }
         break;
      default:
         assert(false);
      }
      break;

   case T_NEW:
      json_append_member(expr_node, "cls", json_mkstring("new"));
      json_append_member(expr_node, "op", dump_expr(tree_value(t)));
      break;

   case T_ALL:
      json_append_member(expr_node, "cls", json_mkstring("all"));
      json_append_member(expr_node, "expr", dump_expr(tree_value(t)));
      break;

   case T_AGGREGATE:
      json_append_member(expr_node, "cls", json_mkstring("aggregate"));
      JsonNode *elements = json_mkarray();
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         tree_t a = tree_assoc(t, i);
         tree_t value = tree_value(a);
         JsonNode *aggreg = json_mkobject();
         add_lineno(aggreg, t);
         switch (tree_subkind(a)) {
         case A_POS:
            json_append_member(aggreg, "cls", json_mkstring("aggregate_pos"));
            json_append_member(aggreg, "expr", dump_expr(value));
            break;
         case A_NAMED:
            json_append_member(aggreg, "cls", json_mkstring("aggregate_named"));
            json_append_member(aggreg, "l", dump_expr(tree_name(a)));
            json_append_member(aggreg, "expr", dump_expr(value));
            break;
         case A_OTHERS:
            json_append_member(aggreg, "cls", json_mkstring("aggregate_others"));
            json_append_member(aggreg, "expr", dump_expr(value));
            break;
         case A_RANGE:
            json_append_member(aggreg, "cls", json_mkstring("aggregate_range"));
            json_append_member(aggreg, "range", dump_range(tree_range(a, 0)));
            json_append_member(aggreg, "expr", dump_expr(value));
            break;
         default:
            assert(false);
         }
         json_append_element(elements, aggreg);
      }
      json_append_member(expr_node, "elts", elements);
      break;

   case T_REF:
      json_append_member(expr_node, "cls", json_mkstring("ref"));
      json_append_member(expr_node, "name", json_mkstring(istr(tree_ident(tree_ref(t)))));
      break;

   case T_ATTR_REF:
      json_append_member(expr_node, "cls", json_mkstring("attr"));
      json_append_member(expr_node, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(expr_node, "op", dump_expr(tree_name(t)));
      break;

   case T_ARRAY_REF:
      json_append_member(expr_node, "cls", json_mkstring("aref"));
      json_append_member(expr_node, "of", dump_expr(tree_value(t)));
      json_append_member(expr_node, "params", dump_params(t, tree_param, tree_params(t), NULL));
      break;

   case T_ARRAY_SLICE:
      json_append_member(expr_node, "cls", json_mkstring("aslice"));
      json_append_member(expr_node, "of", dump_expr(tree_value(t)));
      json_append_member(expr_node, "range", dump_range(tree_range(t, 0)));
      break;

   case T_RECORD_REF:
      json_append_member(expr_node, "cls", json_mkstring("record"));
      json_append_member(expr_node, "of", dump_expr(tree_value(t)));
      json_append_member(expr_node, "item", json_mkstring(istr(tree_ident(t))));
      break;

   case T_TYPE_CONV:
      json_append_member(expr_node, "cls", json_mkstring("typeconv"));
      json_append_member(expr_node, "type", json_mkstring(istr(tree_ident(tree_ref(t)))));
      json_append_member(expr_node, "expr", dump_expr(tree_value(t)));
      break;

   case T_QUALIFIED:
      json_append_member(expr_node, "cls", json_mkstring("qualified"));
      json_append_member(expr_node, "type", json_mkstring(istr(type_ident(tree_type(t)))));
      json_append_member(expr_node, "expr", dump_expr(tree_value(t)));
      break;

   case T_OPEN:
      json_append_member(expr_node, "cls", json_mkstring("open"));
      break;

   default:
      cannot_dump(t, "expr");
   }
   return expr_node;
}

static JsonNode *dump_type(type_t type)
{
   JsonNode *type_node = json_mkobject();
   char *type_name LOCAL = xstrdup(type_pp(type));

   if (type_kind(type) == T_SUBTYPE && type_has_ident(type))
      json_append_member(type_node, "name", json_mkstring(type_name));
   else if (type_is_array(type) && !type_is_unconstrained(type)) {
      json_append_member(type_node, "name", json_mkstring(type_name));
      const int ndims = dimension_of(type);
      JsonNode *range = json_mkarray();
      json_append_member(type_node, "range", range);
      for (int i = 0; i < ndims; i++) {
         tree_t r = range_of(type, i);
         JsonNode *range_obj = dump_range(r);
         json_append_member(range_obj, "cls", json_mkstring("type_range"));
         json_append_element(range, range_obj);
      }
   }
   else
      json_append_member(type_node, "name", json_mkstring(type_name));
   return type_node;
}

static JsonNode *dump_ports(tree_t t, int indent)
{
   JsonNode *ports = json_mkarray();
   const int nports = tree_ports(t);
   if (nports > 0) {
      for (int i = 0; i < nports; i++) {
         json_append_element(ports, dump_port(tree_port(t, i)));
      }
   }
   return ports;
}

static JsonNode *dump_block(tree_t t)
{
   JsonNode *blkobj = json_mkobject();
   add_lineno(blkobj, t);
   JsonNode *decl = json_mkarray();
   const int ndecls = tree_decls(t);
   for (unsigned i = 0; i < ndecls; i++)
      json_append_element(decl, dump_decl(tree_decl(t, i)));
   json_append_member(blkobj, "decl", decl);

   JsonNode *stmts = json_mkarray();
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      json_append_element(stmts, dump_stmt(tree_stmt(t, i)));
   json_append_member(blkobj, "stmts", stmts);
   return blkobj;
}

static JsonNode *dump_decl(tree_t t)
{
   JsonNode *decl = json_mkobject();
   add_lineno(decl, t);
   switch (tree_kind(t)) {
   case T_SIGNAL_DECL:
      json_append_member(decl, "cls", json_mkstring("sigdecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      break;

   case T_VAR_DECL:
      json_append_member(decl, "cls", json_mkstring("vardecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      break;

   case T_CONST_DECL:
      json_append_member(decl, "cls", json_mkstring("constdecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      break;

   case T_TYPE_DECL:
      {
         json_append_member(decl, "cls", json_mkstring("typedecl"));
         type_t type = tree_type(t);
         type_kind_t kind = type_kind(type);
         bool is_subtype = (kind == T_SUBTYPE);

         json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
         json_append_member(decl, "kind", json_mknumber(kind));

         if (is_subtype) {
            json_append_member(decl, "subtype_name", json_mkstring(istr(type_ident(type_base(type)))));
         }

         if (type_is_integer(type) || type_is_real(type)) {
            json_append_member(decl, "range", dump_range(type_dim(type, 0)));
         }
         else if (type_is_physical(type)) {
            json_append_member(decl, "range", dump_range(type_dim(type, 0)));
            JsonNode *units = json_mkarray();
            {
               const int nunits = type_units(type);
               for (int i = 0; i < nunits; i++) {
                  JsonNode *unit = json_mkobject();
                  tree_t u = type_unit(type, i);
                  json_append_member(unit, "type", json_mkstring(istr(tree_ident(u))));
                  json_append_member(unit, "val", dump_expr(tree_value(u)));
                  json_append_element(units, unit);
               }
            }
            json_append_member(decl, "units", units);
         }
         else if (type_is_array(type)) {
            if (kind == T_ARRAY) {
               JsonNode *ua_types = json_mkarray();
               const int nindex = type_index_constrs(type);
               for (int i = 0; i < nindex; i++) {
                  json_append_element(ua_types, dump_type(type_index_constr(type, i)));
               }
               json_append_member(decl, "ua_types", ua_types);
            }
            else if (kind == T_SUBTYPE) {
#if 0
               tree_t constraint = type_constraint(type);
               const int nranges = tree_ranges(constraint);
               JsonNode *st_constr = json_mkarray();
               for (int i = 0; i < nranges; i++) {
                  json_append_element(st_constr, dump_range(tree_range(constraint, i)));
               }
               json_append_member(decl, "st_constr", st_constr);
#endif
            }
            else {
               JsonNode *dims = json_mkarray();
               const int ndims = type_dims(type);
               for (int i = 0; i < ndims; i++) {
                  if (i > 0) printf(", ");
                  json_append_element(dims, dump_range(type_dim(type, i)));
               }
               json_append_member(decl, "dims", dims);
            }
            if (!is_subtype) {
               json_append_member(decl, "of", dump_type(type_elem(type)));
            }
         }
         else if (type_is_protected(type)) {
            JsonNode *prot = json_mkarray();
            for (unsigned i = 0; i < type_decls(type); i++)
               json_append_element(prot, dump_decl(type_decl(type, i)));

            json_append_member(decl, "prot", prot);
         }
         else if (kind == T_ENUM) {
            JsonNode *enum_val = json_mkarray();
            for (unsigned i = 0; i < type_enum_literals(type); i++) {
               json_append_element(enum_val, json_mkstring(istr(tree_ident(type_enum_literal(type, i)))));
            }
            json_append_member(decl, "enum_val", enum_val);
         }
         else
            json_append_member(decl, "type", dump_type(type));
      }
      return decl;

   case T_SPEC:
      json_append_member(decl, "cls", json_mkstring("specdecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      return decl;

   case T_BLOCK_CONFIG:
      json_append_member(decl, "cls", json_mkstring("blk_config"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "decls", dump_decls(t));
      return decl;

   case T_ALIAS:
      json_append_member(decl, "cls", json_mkstring("alias"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "type", dump_type(tree_type(t)));
      json_append_member(decl, "value", dump_expr(tree_value(t)));
      return decl;

   case T_ATTR_SPEC:
      json_append_member(decl, "cls", json_mkstring("attr_spec"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "of", json_mkstring(istr(tree_ident2(t))));
      json_append_member(decl, "class", json_mkstring(class_str(tree_class(t))));
      json_append_member(decl, "value", dump_expr(tree_value(t)));
      return decl;

   case T_ATTR_DECL:
      json_append_member(decl, "cls", json_mkstring("attr_decl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "type", dump_type(tree_type(t)));
      return decl;

   case T_GENVAR:
      json_append_member(decl, "cls", json_mkstring("genvar"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "type", dump_type(tree_type(t)));
      return decl;

   case T_FUNC_DECL:
      json_append_member(decl, "cls", json_mkstring("fdecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "ports", dump_ports(t, 0));
      json_append_member(decl, "ret_type", json_mkstring(type_pp(type_result(tree_type(t)))));
      return decl;

   case T_FUNC_BODY:
      json_append_member(decl, "cls", json_mkstring("fbody"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "ports", dump_ports(t, 0));
      json_append_member(decl, "ret_type", json_mkstring(type_pp(type_result(tree_type(t)))));
      json_append_member(decl, "stmts", dump_block(t));
      return decl;

   case T_PROC_DECL:
      json_append_member(decl, "cls", json_mkstring("pdecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "ports", dump_ports(t, 0));
      //dump_wait_level(t);
      return decl;

   case T_PROC_BODY:
      json_append_member(decl, "cls", json_mkstring("pbody"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "ports", dump_ports(t, 0));
      //dump_wait_level(t);
      json_append_member(decl, "stmts", dump_block(t));
      return decl;

   case T_HIER:
      json_append_member(decl, "cls", json_mkstring("hier"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      return decl;

   case T_COMPONENT:
      json_append_member(decl, "cls", json_mkstring("component"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      JsonNode *generic_array = json_mkarray();
      if (tree_generics(t) > 0) {
         for (unsigned i = 0; i < tree_generics(t); i++) {
            json_append_element(generic_array, dump_port(tree_generic(t, i)));
         }
      }
      json_append_member(decl, "generic", generic_array);

      JsonNode *port_array = json_mkarray();
      if (tree_ports(t) > 0) {
         for (unsigned i = 0; i < tree_ports(t); i++) {
            json_append_element(port_array, dump_port(tree_port(t, i)));
         }
      }
      json_append_member(decl, "port", port_array);
      json_append_member(decl, "decls", dump_decls(t));

      JsonNode *stmts_array = json_mkarray();
      for (unsigned i = 0; i < tree_stmts(t); i++) {
         json_append_element(stmts_array, dump_stmt(tree_stmt(t, i)));
      }
      json_append_member(decl, "stmts", stmts_array);
      return decl;

   case T_PROT_BODY:
      json_append_member(decl, "cls", json_mkstring("prot_body"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));

      JsonNode *decls = json_mkarray();
      for (unsigned i = 0; i < tree_decls(t); i++)
         json_append_element(decls, dump_decl(tree_decl(t, i)));
      json_append_member(decl, "decls", decls);
      return decl;

   case T_FILE_DECL:
      json_append_member(decl, "cls", json_mkstring("file_decl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));
      json_append_member(decl, "type", dump_type(tree_type(t)));
      if (tree_has_value(t)) {
         json_append_member(decl, "open", dump_expr(tree_file_mode(t)));
         json_append_member(decl, "is", dump_expr(tree_value(t)));
      } else {
         json_append_member(decl, "open", json_mknull());
         json_append_member(decl, "is", json_mknull());
      }
      return decl;

   case T_USE:
      json_append_member(decl, "cls", json_mkstring("usedecl"));
      json_append_member(decl, "name", json_mkstring(istr(tree_ident(t))));

      if (tree_has_ident2(t)) {
         json_append_member(decl, "elt", json_mkstring(istr(tree_ident2(t))));
      } else {
         json_append_member(decl, "elt", json_mknull());
      }
      return decl;

   default:
      cannot_dump(t, "decl");
   }

   json_append_member(decl, "type", dump_type(tree_type(t)));

   if (tree_has_value(t))
      json_append_member(decl, "val", dump_expr(tree_value(t)));
   else
      json_append_member(decl, "val", json_mknull());

   return decl;
}

static JsonNode *dump_stmt(tree_t t)
{
   JsonNode *statement = json_mkobject();
   add_lineno(statement, t);
   switch (tree_kind(t)) {
   case T_PROCESS:
      {
         JsonNode *stmt = json_mkarray();
         json_append_member(statement, "cls", json_mkstring("process"));
         json_append_member(statement, "decls", dump_decls(t));
         json_append_member(statement, "stmts", stmt);
         for (unsigned i = 0; i < tree_stmts(t); i++)
            json_append_element(stmt, dump_stmt(tree_stmt(t, i)));
      }
      break;

   case T_SIGNAL_ASSIGN:
      json_append_member(statement, "cls", json_mkstring("sigassign"));
      json_append_member(statement, "target", dump_expr(tree_target(t)));
      /* Delays are not translated */
      if (tree_waveforms(t)) {
         tree_t w = tree_waveform(t, 0);
         json_append_member(statement, "lhs", dump_expr(tree_value(w)));
      }
      break;

   case T_VAR_ASSIGN:
      json_append_member(statement, "cls", json_mkstring("varassign"));
      json_append_member(statement, "target", dump_expr(tree_target(t)));
      json_append_member(statement, "lhs", dump_expr(tree_value(t)));
      break;

   case T_WAIT:
      json_append_member(statement, "cls", json_mkstring("wait"));
      JsonNode *wait_on = json_mkarray();
      json_append_member(statement, "on", wait_on);
      if (tree_triggers(t) > 0) {
         for (unsigned i = 0; i < tree_triggers(t); i++) {
            json_append_element(wait_on, dump_expr(tree_trigger(t, i)));
         }
      }
      break;

   case T_BLOCK:
      json_append_member(statement, "cls", json_mkstring("block"));
      json_append_member(statement, "block", dump_block(t));
      break;

   case T_ASSERT:
      json_append_member(statement, "cls", json_mkstring("assert"));

      if (tree_has_value(t)) {
         json_append_member(statement, "cond", dump_expr(tree_value(t)));
      } else {
         json_append_member(statement, "cond", json_mknull());
      }

      if (tree_has_message(t)) {
         json_append_member(statement, "message", dump_expr(tree_message(t)));
      } else {
         json_append_member(statement, "message", json_mknull());
      }
      json_append_member(statement, "severity", dump_expr(tree_severity(t)));
      break;

   case T_WHILE:
      json_append_member(statement, "cls", json_mkstring("while"));

      if (tree_has_value(t)) {
         json_append_member(statement, "cond", dump_expr(tree_value(t)));
      } else {
         json_append_member(statement, "cond", json_mknull());
      }
      JsonNode *stmts = json_mkarray();
      for (unsigned i = 0; i < tree_stmts(t); i++)
         json_append_element(stmts, dump_stmt(tree_stmt(t, i)));

      json_append_member(statement, "stmts", stmts);
      break;

   case T_IF:
      json_append_member(statement, "cls", json_mkstring("if"));
      JsonNode *conds = json_mkarray();
      json_append_member(statement, "conds", conds);
      for (unsigned i = 0; i < tree_conds(t); i++) {
         tree_t c = tree_cond(t, i);
         JsonNode *cond = json_mkobject();
         json_append_member(cond, "cls", json_mkstring("cond"));
         if (tree_has_value(c))
            json_append_member(cond, "value", dump_expr(tree_value(c)));
         JsonNode *stmts = json_mkarray();
         json_append_member(cond, "stmts", stmts);
         for (unsigned j = 0; j < tree_stmts(c); j++)
            json_append_element(stmts, dump_stmt(tree_stmt(c, i)));
         json_append_element(conds, cond);
      }
      break;

   case T_EXIT:
      json_append_member(statement, "cls", json_mkstring("exit"));
      json_append_member(statement, "name", json_mkstring(istr(tree_ident2(t))));

      if (tree_has_value(t)) {
         json_append_member(statement, "when", dump_expr(tree_value(t)));
      } else {
         json_append_member(statement, "when", json_mknull());
      }
      break;

   case T_CASE:
      json_append_member(statement, "cls", json_mkstring("case"));
      json_append_member(statement, "sel", dump_expr(tree_value(t)));
      JsonNode *assocs = json_mkarray();
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         tree_t a = tree_assoc(t, i);
         JsonNode *assoc = json_mkobject();
         switch (tree_subkind(a)) {
         case A_NAMED:
            json_append_member(assoc, "cls", json_mkstring("expr"));
            json_append_member(assoc, "expr", dump_expr(tree_name(a)));
            break;
         case A_OTHERS:
            json_append_member(assoc, "cls", json_mkstring("others"));
            break;
         default:
            assert(false);
         }
         json_append_member(assoc, "to", dump_stmt(tree_value(a)));
         json_append_element(assocs, assoc);
      }
      json_append_member(statement, "assoc", assocs);
      break;

   case T_RETURN:
      json_append_member(statement, "cls", json_mkstring("return"));
      if (tree_has_value(t)) {
         json_append_member(statement, "expr", dump_expr(tree_value(t)));
      } else {
         json_append_member(statement, "expr", json_mknull());
      }
      break;

   case T_FOR:
   {
      json_append_member(statement, "cls", json_mkstring("for"));
      json_append_member(statement, "name", json_mkstring(istr(tree_ident2(t))));
      json_append_member(statement, "range", dump_range(tree_range(t, 0)));
      JsonNode *stmts = json_mkarray();
      for (unsigned i = 0; i < tree_stmts(t); i++)
         json_append_element(stmts, dump_stmt(tree_stmt(t, i)));

      json_append_member(statement, "stmts", stmts);
   }
      break;

   case T_PCALL:
      json_append_member(statement, "cls", json_mkstring("pcall"));
      json_append_member(statement, "name", json_mkstring(istr(tree_ident(tree_ref(t)))));
      json_append_member(statement, "params", dump_params(t, tree_param, tree_params(t), NULL));
      break;

   case T_FOR_GENERATE:
      {
         json_append_member(statement, "cls", json_mkstring("for_generate"));
         json_append_member(statement, "name", json_mkstring(istr(tree_ident2(t))));
         json_append_member(statement, "range", dump_range(tree_range(t, 0)));

         JsonNode *decls = json_mkarray();
         for (unsigned i = 0; i < tree_decls(t); i++)
            json_append_element(decls, dump_decl(tree_decl(t, i)));

         JsonNode *stmts = json_mkarray();
         for (unsigned i = 0; i < tree_stmts(t); i++)
            json_append_element(stmts, dump_stmt(tree_stmt(t, i)));
         json_append_member(statement, "decls", decls);
         json_append_member(statement, "stmts", stmts);
      }
      break;

   case T_IF_GENERATE:
   {
      json_append_member(statement, "cls", json_mkstring("if_generate"));
      json_append_member(statement, "cond", dump_expr(tree_value(t)));

      JsonNode *decls = json_mkarray();
      for (unsigned i = 0; i < tree_decls(t); i++)
         json_append_element(decls, dump_decl(tree_decl(t, i)));

      JsonNode *stmts = json_mkarray();
      for (unsigned i = 0; i < tree_stmts(t); i++)
         json_append_element(stmts, dump_stmt(tree_stmt(t, i)));
      json_append_member(statement, "decls", decls);
      json_append_member(statement, "stmts", stmts);
   }
   break;

   case T_INSTANCE:
      json_append_member(statement, "cls", json_mkstring("instance"));
      switch (tree_class(t)) {
      case C_ENTITY:    json_append_member(statement, "inst_type", json_mkstring("entity")); break;
      case C_COMPONENT: json_append_member(statement, "inst_type", json_mkstring("component")); break;
      default:
         assert(false);
      }
      json_append_member(statement, "name", json_mkstring(istr(tree_ident2(t))));
      json_append_member(statement, "iname", json_mkstring(istr(tree_ident(t))));
      if (tree_has_spec(t)) {
         tree_t bind = tree_value(tree_spec(t));
         json_append_member(statement, "bound_to", json_mkstring(istr(tree_ident(bind))));
         if (tree_has_ident2(bind))
            json_append_member(statement, "bound_from", json_mkstring(istr(tree_ident2(bind))));
         else {
            json_append_member(statement, "bound_from", json_mknull());
         }

      } else {
         json_append_member(statement, "bound_to", json_mknull());
         json_append_member(statement, "bound_from", json_mknull());
      }

      if (tree_genmaps(t) > 0) {
         json_append_member(statement, "generic", dump_params(t, tree_genmap, tree_genmaps(t), ""));
      }

      if (tree_params(t) > 0) {
         json_append_member(statement, "port", dump_params(t, tree_param, tree_params(t), ""));
      }
      break;

   case T_NEXT:
      json_append_member(statement, "cls", json_mkstring("next"));
      if (tree_has_value(t)) {
         json_append_member(statement, "when", dump_expr(tree_value(t)));
      } else {
         json_append_member(statement, "when", json_mknull());
      }

      break;

   default:
      cannot_dump(t, "stmt");
   }

   return statement;
}

static JsonNode *dump_port(tree_t t)
{
   JsonNode *port = json_mkobject();
   add_lineno(port, t);
   json_append_member(port, "name", json_mkstring(istr(tree_ident(t))));

   const char *class = NULL, *dir = NULL;
   switch (tree_class(t)) {
   case C_SIGNAL:   class = "signal";   break;
   case C_VARIABLE: class = "variable"; break;
   case C_DEFAULT:  class = "";         break;
   case C_CONSTANT: class = "constant"; break;
   case C_FILE:     class = "file";     break;
   default:
      assert(false);
   }
   json_append_member(port, "class", json_mkstring(class));
   switch (tree_subkind(t)) {
   case PORT_IN:      dir = "in";     break;
   case PORT_OUT:     dir = "out";    break;
   case PORT_INOUT:   dir = "inout";  break;
   case PORT_BUFFER:  dir = "buffer"; break;
   case PORT_INVALID: dir = "??";     break;
   }
   json_append_member(port, "dir", json_mkstring(dir));
   json_append_member(port, "type", dump_type(tree_type(t)));
   if (tree_has_value(t)) {
      json_append_member(port, "val", dump_expr(tree_value(t)));
   }
   return port;
}

static JsonNode *dump_context(tree_t t)
{
   JsonNode *ctx = json_mkarray();
   const int nctx = tree_contexts(t);
   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(t, i);

      switch (tree_kind(c)) {
      case T_LIBRARY:
         if (tree_ident(c) != well_known(W_STD) && tree_ident(c) != well_known(W_WORK)) {
            JsonNode *library = json_mkobject();
            add_lineno(library, c);
            json_append_member(library, "cls", json_mkstring("library"));
            json_append_member(library, "name", json_mkstring(istr(tree_ident(c))));
            json_append_element(ctx, library);
         }
         break;

      case T_USE:
      {
            JsonNode *use = json_mkobject();
            add_lineno(use, c);
            json_append_member(use, "cls", json_mkstring("use"));
            json_append_member(use, "name", json_mkstring(istr(tree_ident(c))));
         if (tree_has_ident2(c)) {
            json_append_member(use, "elt", json_mkstring(istr(tree_ident2(c))));
         } else {
            json_append_member(use, "elt", json_mknull());
         }
         json_append_element(ctx, use);
      }
         break;

      default:
         break;
      }
   }
   return ctx;
}

static JsonNode *dump_entity(tree_t t)
{
   JsonNode *entity_node = json_mkobject();
   add_lineno(entity_node, t);
   json_append_member(entity_node, "cls", json_mkstring("entity"));
   json_append_member(entity_node, "ctx", dump_context(t));
   add_filename(entity_node, t);
   json_append_member(entity_node, "name", json_mkstring(istr(tree_ident(t))));
   JsonNode *generic_array = json_mkarray();
   if (tree_generics(t) > 0) {
      for (unsigned i = 0; i < tree_generics(t); i++) {
         json_append_element(generic_array, dump_port(tree_generic(t, i)));
      }
   }
   json_append_member(entity_node, "generic", generic_array);

   JsonNode *port_array = json_mkarray();
   if (tree_ports(t) > 0) {
      for (unsigned i = 0; i < tree_ports(t); i++) {
         json_append_element(port_array, dump_port(tree_port(t, i)));
      }
   }
   json_append_member(entity_node, "port", port_array);
   json_append_member(entity_node, "decls", dump_decls(t));

   JsonNode *stmts_array = json_mkarray();
   for (unsigned i = 0; i < tree_stmts(t); i++) {
      json_append_element(stmts_array, dump_stmt(tree_stmt(t, i)));
   }
   json_append_member(entity_node, "stmts", stmts_array);
   return entity_node;
}

static JsonNode *dump_decls(tree_t t)
{
   JsonNode *decls = json_mkarray();
   const int ndecls = tree_decls(t);
   for (unsigned i = 0; i < ndecls; i++)
      json_append_element(decls, dump_decl(tree_decl(t, i)));
   return decls;
}

static JsonNode *dump_arch(tree_t t)
{
   JsonNode *architecture_node = json_mkobject();
   add_lineno(architecture_node, t);
   json_append_member(architecture_node, "cls", json_mkstring("architecture"));
   json_append_member(architecture_node, "ctx", dump_context(t));
   add_filename(architecture_node, t);
   json_append_member(architecture_node, "name", json_mkstring(istr(tree_ident(t))));
   json_append_member(architecture_node, "of", json_mkstring(istr(tree_ident2(t))));
   json_append_member(architecture_node, "decls", dump_decls(t));
   JsonNode *stmts_array = json_mkarray();
   for (unsigned i = 0; i < tree_stmts(t); i++)
      json_append_element(stmts_array, dump_stmt(tree_stmt(t, i)));
   json_append_member(architecture_node, "stmts", stmts_array);
   return architecture_node;
}

static JsonNode *dump_package(tree_t t)
{
   JsonNode *pkg_node = json_mkobject();
   add_lineno(pkg_node, t);
   json_append_member(pkg_node, "cls", json_mkstring("pkg"));
   json_append_member(pkg_node, "ctx", dump_context(t));
   add_filename(pkg_node, t);
   json_append_member(pkg_node, "name", json_mkstring(istr(tree_ident(t))));
   json_append_member(pkg_node, "decls", dump_decls(t));
   return pkg_node;
}

static JsonNode *dump_package_body(tree_t t)
{
   JsonNode *pkg_node = json_mkobject();
   add_lineno(pkg_node, t);
   json_append_member(pkg_node, "cls", json_mkstring("pkg_body"));
   json_append_member(pkg_node, "ctx", dump_context(t));
   add_filename(pkg_node, t);
   json_append_member(pkg_node, "name", json_mkstring(istr(tree_ident(t))));
   json_append_member(pkg_node, "decls", dump_decls(t));
   return pkg_node;
}

static JsonNode *dump_configuration(tree_t t)
{
   JsonNode *config_node = json_mkobject();
   add_lineno(config_node, t);
   json_append_member(config_node, "cls", json_mkstring("config"));
   json_append_member(config_node, "name", json_mkstring(istr(tree_ident(t))));
   json_append_member(config_node, "of", json_mkstring(istr(tree_ident2(t))));
   json_append_member(config_node, "decls", dump_decls(t));
   return config_node;
}

JsonNode *trees_to_json(tree_t *elements, unsigned int n_elements)
{
   unsigned int i;
   JsonNode *base_node = json_mkarray();
   for(i=0; i < n_elements; i++) {
      tree_t t = elements[i];
      switch (tree_kind(t)) {
      case T_ENTITY:
         json_append_element(base_node, dump_entity(t));
         break;
      case T_ARCH:
         json_append_element(base_node, dump_arch(t));
         break;
      case T_PACKAGE:
         json_append_element(base_node, dump_package(t));
         break;
      case T_PACK_BODY:
         json_append_element(base_node, dump_package_body(t));
         break;
      case T_CONFIGURATION:
         json_append_element(base_node, dump_configuration(t));
         break;
      default:
         cannot_dump(t, "tree");
      }
   }

   return base_node;
}

void dump_json(tree_t *elements, unsigned int n_elements, const char *filename)
{
   FILE* dump_file = fopen(filename, "w");
   if (!dump_file) {
      fatal_errno("Failed to open JSON file %s", filename);
      return;
   }

   JsonNode *base_node = trees_to_json(elements, n_elements);
   char *result = json_encode(base_node);
   fwrite(result, 1, strlen(result), dump_file);
   fclose(dump_file);
   json_delete(base_node);
}
