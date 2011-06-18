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
   
   #include <sys/types.h>
   #include <sys/mman.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <unistd.h>
}

%code requires {
   #include "tree.h"
   
   #include <stdbool.h>
   
   #define YYLTYPE YYLTYPE
   typedef struct YYLTYPE {
      int  first_line;
      int  first_column;
      int  last_line;
      int  last_column;
      char *filename;
   } YYLTYPE;

   typedef struct {
      int  ival;
      char *sval;
   } lvals_t;
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
   
   static int        n_errors = 0;
   static const char *read_ptr;
   static const char *file_start;
   static size_t     file_sz;
   static const char *perm_linebuf = NULL;
   static int        n_chars_this_line = 0;
   static int        n_token_next_start = 0;
   static int        n_row = 0;
   static bool       last_was_newline = true;
   static tree_t     root;

   int yylex(void);
   static void yyerror(const char *s);
}

%union {
   tree_t t;
}

%type <t> entity

%token tID tENTITY tIS tEND tGENERIC tPORT tCONSTANT tCOMPONENT
%token tCONFIGURATION tARCHITECTURE tOF tBEGIN tAND tOR tXOR tXNOR
%token tNAND tABS tNOT tALL tIN tOUT tBUFFER tBUS tUNAFFECTED tSIGNAL
%token tPROCESS tWAIT tREPORT tLPAREN tRPAREN tSEMI tASSIGN tCOLON
%token tPOWER tCOMMA tLE tINT tSTRING tCHAR tERROR

%error-verbose
%expect 0

%%

root
: entity { root = $1; }
| /* empty */ {} 
;

entity
: tENTITY { $$ = tree_new(T_ENTITY); }
;

%%

static void yyerror(const char *s)
{
   fprintf(stderr, "%s\n", s);
   n_errors++;
}

void begin_token(char *tok)
{

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

   read_ptr = file_start;
   last_was_newline = true;
   
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
