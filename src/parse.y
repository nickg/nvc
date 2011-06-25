//
//  Copyright (C) 2011  Nick Gasson
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

%code top {
   #include "util.h"
   #include "ident.h"
   
   #include <sys/types.h>
   #include <sys/mman.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <unistd.h>
   #include <string.h>
}

%code requires {
   #include "tree.h"
   
   #include <stdbool.h>
   
   #define YYLTYPE YYLTYPE
   typedef struct YYLTYPE {
      int        first_line;
      int        first_column;
      int        last_line;
      int        last_column;
      const char *file;
      const char *linebuf;
   } YYLTYPE;

   #define YYLLOC_DEFAULT(Current, Rhs, N) {                         \
         if (N) {                                                    \
            (Current).first_line   = YYRHSLOC(Rhs, 1).first_line;    \
            (Current).first_column = YYRHSLOC(Rhs, 1).first_column;  \
            (Current).last_line    = YYRHSLOC(Rhs, N).last_line;     \
            (Current).last_column  = YYRHSLOC(Rhs, N).last_column;   \
            (Current).file         = YYRHSLOC(Rhs, N).file;          \
            (Current).linebuf      = YYRHSLOC(Rhs, 1).linebuf;       \
         }                                                           \
         else {                                                      \
            (Current).first_line   = (Current).last_line   =         \
               YYRHSLOC(Rhs, 0).last_line;                           \
            (Current).first_column = (Current).last_column =         \
               YYRHSLOC(Rhs, 0).last_column;                         \
            (Current).file = YYRHSLOC(Rhs, 0).file;                  \
            (Current).linebuf = YYRHSLOC(Rhs, 0).linebuf;            \
         }                                                           \
      }
   
   typedef struct {
      int  ival;
      char *sval;
   } lvals_t;
   
   typedef struct id_list {
      struct id_list *next;
      ident_t        id;
   } id_list_t;
   
   typedef struct tree_list {
      struct tree_list *next;
      tree_t           value;
   } tree_list_t;
}

%code provides {
   bool input_from_file(const char *file);
   
   tree_t parse(void);
   void begin_token(char *tok);
   int get_next_char(char *b, int max_buffer);
   int parse_errors(void);
}

%code {
   extern lvals_t lvals;
   extern YYLTYPE yylloc;
   
   static int        n_errors = 0;
   static const char *read_ptr;
   static const char *file_start;
   static size_t     file_sz;
   static const char *perm_linebuf = NULL;
   static const char *perm_file_name = NULL;
   static int        n_chars_this_line = 0;
   static int        n_token_next_start = 0;
   static int        n_row = 0;
   static bool       last_was_newline = true;
   static tree_t     root;

   int yylex(void);
   static void yyerror(const char *s);

   static void copy_trees(tree_list_t *from,
                          void (*copy_fn)(tree_t t, tree_t d),
                          tree_t to);
   static id_list_t *id_list_add(id_list_t *list, ident_t id);
   static void id_list_free(id_list_t *list);
   static void tree_list_append(tree_list_t **l, tree_t t);
   static void tree_list_prepend(tree_list_t **l, tree_t t);
   static void tree_list_concat(tree_list_t **a, tree_list_t *b);
   static void tree_list_free(tree_list_t *l);
}

%union {
   tree_t      t;
   ident_t     i;
   tree_list_t *l;
   struct {
      tree_list_t *left;
      tree_list_t *right;
   } p;
   id_list_t   *s;
   port_mode_t m;
   type_t      y;
}

%type <t> entity_decl 
%type <i> id opt_id name simple_name
%type <l> interface_signal_decl interface_object_decl interface_list
%type <l> port_clause generic_clause interface_decl
%type <p> entity_header
%type <s> id_list;
%type <m> opt_mode
%type <y> subtype_indication type_mark

%token tID tENTITY tIS tEND tGENERIC tPORT tCONSTANT tCOMPONENT
%token tCONFIGURATION tARCHITECTURE tOF tBEGIN tAND tOR tXOR tXNOR
%token tNAND tABS tNOT tALL tIN tOUT tBUFFER tBUS tUNAFFECTED tSIGNAL
%token tPROCESS tWAIT tREPORT tLPAREN tRPAREN tSEMI tASSIGN tCOLON
%token tPOWER tCOMMA tLE tINT tSTRING tCHAR tERROR tINOUT tLINKAGE

%error-verbose
%expect 0

%%

root
: entity_decl { root = $1; YYACCEPT; }
| /* empty */ {} 
;

id
: tID
  {
     $$ = ident_new(lvals.sval);
     free(lvals.sval);
  }
;

id_list
: id
  {
     $$ = id_list_add(NULL, $1);
  }
| id tCOMMA id_list
  {
     $$ = id_list_add($3, $1);
  }
;

opt_id : id { $$ = $1; } | { $$ = NULL; } ;

entity_decl
: tENTITY id tIS entity_header /* entity_decl_part */ tEND
  opt_entity_token opt_id tSEMI
  {
     $$ = tree_new(T_ENTITY);
     tree_set_ident($$, $2);
     copy_trees($4.left, tree_add_generic, $$);
     copy_trees($4.right, tree_add_port, $$);
  }
;

opt_entity_token : tENTITY {} | {} ;

entity_header
: generic_clause port_clause
  {
     $$.left  = $1;
     $$.right = $2;
  }
;

generic_clause
: tGENERIC tLPAREN interface_list tRPAREN tSEMI { $$ = $3; }
| /* empty */ { $$ = NULL; }
;

port_clause
: tPORT tLPAREN interface_list tRPAREN tSEMI { $$ = $3; }
| /* empty */ { $$ = NULL; }
;

interface_list
: interface_decl
  {
     $$ = $1;
  }
| interface_decl tSEMI interface_list
  {
     $$ = $1;
     tree_list_concat(&$$, $3);
  }
;

interface_decl
: interface_object_decl
  /* | interface_type_declaration
     | interface_subprogram_declaration
     | interface_package_declaration */
;

interface_object_decl
: interface_signal_decl
  /* | interface_constant_declaration
     | interface_variable_declaration
     | interface_file_declaration */
;

interface_signal_decl
: opt_signal_token id_list tCOLON opt_mode subtype_indication
  /* opt_bus_token [ := static_expression ] */
  {
     $$ = NULL;
     for (id_list_t *it = $2; it != NULL; it = it->next) {
        tree_t t = tree_new(T_PORT_DECL);
        tree_set_ident(t, it->id);
        tree_set_port_mode(t, $4);
        tree_set_type(t, $5);

        tree_list_prepend(&$$, t);
     }

     id_list_free($2);
  }
;

opt_signal_token : tSIGNAL | /* empty */ ;

opt_mode
: tIN { $$ = PORT_IN; }
| tOUT { $$ = PORT_OUT; }
| tINOUT { $$ = PORT_INOUT; }
| tBUFFER { $$ = PORT_BUFFER; }
| /* empty */ { $$ = PORT_IN; }
;

subtype_indication
: /* resolution_indication */ type_mark /* constraint */
  { $$ = $1; }
;

type_mark
: name
  {
     $$ = type_new(T_UNRESOLVED);
     type_set_ident($$, $1);
  }
;

name
: simple_name
  /* | operator_symbol
     | character_literal
     | selected_name
     | indexed_name
     | slice_name
     | attribute_name
     | external_name */
;

simple_name : id ;

%%

static void tree_list_append(struct tree_list **l, tree_t t)
{
   struct tree_list *new = xmalloc(sizeof(struct tree_list));
   new->next  = NULL;
   new->value = t;

   tree_list_concat(l, new);
}

static void tree_list_prepend(struct tree_list **l, tree_t t)
{
   struct tree_list *new = xmalloc(sizeof(struct tree_list));
   new->next  = *l;
   new->value = t;
   
   *l = new;
}

static void tree_list_concat(struct tree_list **a, struct tree_list *b)
{
   if (*a == NULL)
      *a = b;
   else {
      struct tree_list *it;
      for (it = *a; it->next != NULL; it = it->next)
         ;
      it->next = b;
   }
}

static void tree_list_free(struct tree_list *l)
{
   while (l != NULL) {
      struct tree_list *next = l->next;
      free(l);
      l = next;
   }
}

static void copy_trees(tree_list_t *from,
                       void (*copy_fn)(tree_t t, tree_t d),
                       tree_t to)
{
   for (; from != NULL; from = from->next)
      (*copy_fn)(to, from->value);
   tree_list_free(from);
}

static id_list_t *id_list_add(id_list_t *list, ident_t id)
{
   id_list_t *new = xmalloc(sizeof(id_list_t));
   new->next = list;
   new->id   = id;
   return new;
}

static void id_list_free(id_list_t *list)
{
   while (list != NULL) {
      id_list_t *next = list->next;
      free(list);
      list = next;
   }      
}      

static void yyerror(const char *s)
{
   fprintf(stderr, "%s:%d: %s\n", yylloc.file, yylloc.first_line, s);

   const char *lb = yylloc.linebuf;
   char buf[80];
   size_t i = 0;
   while (i < sizeof(buf) - 4 && *lb != '\0' && *lb != '\n') {
      // TODO: expand tabs?
      buf[i++] = *lb++;
   }

   if (i == sizeof(buf) - 4) {
      buf[i++] = '.';
      buf[i++] = '.';
      buf[i++] = '.';
   }

   buf[i] = '\0';

   // Print ... if error location spans multiple lines
   bool many_lines = (yylloc.first_line != yylloc.last_line);
   int last_col = many_lines ? strlen(buf) + 4 : yylloc.last_column;

   fprintf(stderr, "    %s%s\n", buf, many_lines ? " ..." : "");
   for (int i = 0; i < yylloc.first_column + 4; i++)
      fprintf(stderr, " ");
   for (int i = 0; i < last_col - yylloc.first_column + 1; i++)
      fprintf(stderr, "^");
   fprintf(stderr, "\n");
   
   n_errors++;
}

void begin_token(char *tok)
{
   const char *newline = strchr(tok, '\n');
   int n_token_start, n_token_length;
   if (newline != NULL) {
      n_token_start = 0;
      n_token_length = strlen(tok) - (newline - tok);
      n_token_next_start = n_token_length - 1;
   }
   else {
      n_token_start = n_token_next_start;
      n_token_length = strlen(tok);
      n_token_next_start = n_chars_this_line;
   }
   
   yylloc.first_line   = n_row;
   yylloc.first_column = n_token_start;
   yylloc.last_line    = n_row;
   yylloc.last_column  = n_token_start + n_token_length - 1;
   yylloc.file         = perm_file_name;
   yylloc.linebuf      = perm_linebuf;
}

int get_next_char(char *b, int max_buffer)
{
   if (last_was_newline) {
      n_token_next_start = 0;
      n_chars_this_line = 0;
      n_row += 1;

      perm_linebuf = read_ptr;

      last_was_newline = false;
   }

   const bool eof = read_ptr >= file_start + file_sz;
   if (eof)
      return 0;
   else
      *b = *read_ptr++;
   
   if (perm_linebuf == NULL)
      perm_linebuf = read_ptr;

   n_chars_this_line++;
   
   if (*b == '\n') 
      last_was_newline = true;
   
   return *b == 0 ? 0 : 1;
}

bool input_from_file(const char *file)
{
   int fd = open(file, O_RDONLY);
   if (fd < 0) {
      perror(file);
      return false;
   }

   struct stat buf;
   if (fstat(fd, &buf) != 0) {
      perror("fstat");
      return false;
   }

   file_sz = buf.st_size;

   file_start = mmap(NULL, file_sz, PROT_READ, MAP_PRIVATE, fd, 0);
   if (file_start == MAP_FAILED) {
      perror("mmap");
      return false;
   }

   read_ptr         = file_start;
   last_was_newline = true;
   perm_file_name   = strdup(file);
   
   return true;
}

tree_t parse(void)
{
   root = NULL;
   
   if (yyparse())
      return NULL;
   else
      return root;
}

int parse_errors(void)
{
   return n_errors;
}
