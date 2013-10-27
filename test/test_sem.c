#include "parse.h"
#include "type.h"
#include "phase.h"
#include "util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct error {
   int        line;
   const char *snippet;
} error_t;

static const error_t  *error_lines = NULL;
static error_fn_t orig_error_fn = NULL;

static void setup(void)
{
   lib_set_work(lib_tmp());
   opt_set_int("bootstrap", 0);
}

static void teardown(void)
{
   lib_free(lib_work());
}

static void test_error_fn(const char *msg, const loc_t *loc)
{
   fail_if(error_lines == NULL);

   bool unexpected = error_lines->line == -1
      || error_lines->snippet == NULL
      || error_lines->line != loc->first_line
      || strstr(msg, error_lines->snippet) == NULL;

   if (unexpected) {
      orig_error_fn(msg, loc);
      printf("expected line %d '%s'\n",
             error_lines->line, error_lines->snippet);
   }

   fail_if(unexpected);

   error_lines++;
}

static void expect_errors(const error_t *lines)
{
   fail_unless(orig_error_fn == NULL);
   orig_error_fn = set_error_fn(test_error_fn);
   error_lines = lines;
}

START_TEST(test_integer)
{
   tree_t a, d, p, s, e;
   type_t t;
   range_t r;

   input_from_file(TESTDIR "/sem/integer.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   const error_t expect[] = {
      { 20, "MY_INT1 does not match type of target MY_INT2" },
      { 30, "MY_INT1 does not match type of target MY_INT2_SUB" },
      { 35, "type NOTHING is not defined" },
      { 48, "no suitable overload for operator \"*\"(MY_INT2, MY_INT1)" },
      { 57, "MY_INT2 has no attribute CAKE" },
      { -1, NULL }
   };
   expect_errors(expect);

   sem_check(a);
   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);

   d = tree_decl(a, 2);
   fail_unless(tree_ident(d) == ident_new("X"));
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
   e = tree_value(d);
   fail_unless(tree_kind(e) == T_LITERAL);
   t = tree_type(e);
   fail_unless(type_kind(t) == T_INTEGER);

   fail_unless(tree_stmts(a) == 6);

   // Process 1

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   fail_unless(tree_ident(d) == ident_new("Z"));
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);

   s = tree_stmt(p, 0);
   fail_unless(tree_ref(tree_target(s)) == d);

   // Process 6

   p = tree_stmt(a, 5);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_decls(p) == 1);

   d = tree_decl(p, 0);
   r = type_dim(tree_type(d), 0);

   s = tree_stmt(p, 0);
   fail_unless(tree_kind(tree_value(s)) == T_ATTR_REF);
   fail_unless(tree_value(tree_value(s)) == r.left);
   s = tree_stmt(p, 1);
   fail_unless(tree_kind(tree_value(s)) == T_ATTR_REF);
   fail_unless(tree_value(tree_value(s)) == r.right);
   s = tree_stmt(p, 2);
   fail_unless(tree_kind(tree_value(s)) == T_ATTR_REF);
   fail_unless(tree_value(tree_value(s)) == r.right);

}
END_TEST

START_TEST(test_ports)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/ports.vhd");

   const error_t expect[] = {
      { 31,  "cannot read output port O" },
      { 42,  "cannot assign to input port I" },
      { 81,  "missing actual for formal I" },
      { 85,  "formal I already has an actual" },
      { 89,  "too many positional actuals" },
      { 92,  "WORK.FOO has no formal CAKE" },
      { 94,  "cannot find unit WORK.BAD" },
      { 103, "unconnected port I with mode IN must have a default value" },
      { 116, "object X is not a component declaration" },
      { 148, "actual must be globally static expression" },
      { 155, "undefined identifier Q" },
      { 163, "undefined identifier U" },
      { 168, "formal name must be static" },
      { 177, "formal name must be static" },
      { 185, "undefined identifier HELLO" },
      { 217, "port O of unconstrained type INT_VEC cannot be unconnected" },
      { 221, "type of actual universal real does not match type INTEGER" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   // Entity foo

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   // Entity top

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   // Architecture test

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   // Architecture other

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST


START_TEST(test_scope)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/scope.vhd");

   const error_t expect[] = {
      {  31, "WORK.PACK1.MY_INT1 does not match type"
         " of target WORK.PACK2.MY_INT1" },
      {  44, "WORK.PACK1.MY_INT1 does not match type of target MY_INT1" },
      {  63, "G already declared in this scope" },
      {  71, "P already declared in this scope" },
      { 114, "type MY_INT1 is not defined" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   for (int i = 0; i < 3; i++) {
      a = parse();
      fail_if(a == NULL);
      fail_unless(tree_kind(a) == T_ARCH);
      sem_check(a);
   }

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_ambiguous)
{
   tree_t a, e, p, s;
   type_t lhs, rhs;

   input_from_file(TESTDIR "/sem/ambiguous.vhd");

   const error_t expect[] = {
      { 35,  "type of value BAR does not match type of target FOO" },
      { 56,  "type of aggregate is ambiguous" },
      { 86,  "ambiguous use of enumeration literal FALSE" },
      { 93,  "ambiguous call to function NOW" },
      { 103, "ambiguous use of name FALSE" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   p = tree_stmt(a, 0);
   fail_unless(tree_stmts(p) == 2);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("FOO"));
   fail_unless(type_ident(rhs) == ident_new("FOO"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("BAR"));
   fail_unless(type_ident(rhs) == ident_new("BAR"));

   p = tree_stmt(a, 1);
   fail_unless(tree_stmts(p) == 2);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("FOO"));
   fail_unless(type_ident(rhs) == ident_new("FOO"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("BAR"));
   fail_unless(type_ident(rhs) == ident_new("BAR"));

   p = tree_stmt(a, 2);
   fail_unless(tree_stmts(p) == 3);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("BAZ"));
   fail_unless(type_ident(rhs) == ident_new("BAZ"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("BAZ"));
   fail_unless(type_ident(rhs) == ident_new("BAZ"));
   s = tree_stmt(p, 2);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("FOO"));
   fail_unless(type_ident(rhs) == ident_new("FOO"));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_const)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/const.vhd");

   const error_t expect[] = {
      { 19, "invalid target of variable assignment" },
      { 23, "deferred constant declarations are only permitted" },
      { 53, "constant WORK.P.C already has a value" },
      { 54, "expected type INTEGER for deferred constant WORK.P.F" },
      { 44, "deferred constant WORK.P.D was not given a value" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_std)
{
   tree_t a, e, d;

   input_from_file(TESTDIR "/sem/std.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);
   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_wait)
{
   tree_t a, e, p, w;

   input_from_file(TESTDIR "/sem/wait.vhd");

   const error_t expect[] = {
      { 17, "type of delay must be TIME" },
      { 26, "name V in sensitivity list is not a signal" },
      { 35, "invalid use of A" },
      { 40, "wait statement not allowed in process" },
      { 51, "type of condition must be BOOLEAN" },
      { 53, "type of delay must be TIME" },
      { 66, "name in sensitivity list is not static" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 4);

   ident_t time = ident_new("STD.STANDARD.TIME");

   w = tree_stmt(p, 0);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   w = tree_stmt(p, 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   w = tree_stmt(p, 2);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_func)
{
   tree_t p;

   input_from_file(TESTDIR "/sem/func.vhd");

   const error_t expect[] = {
      {   5, "function arguments must have mode IN" },
      {  17, "must be an unconstrained array type" },
      {  21, "resolution function must have single argument" },
      {  25, "declaration UENUM is not a function" },
      {  27, "type of default value universal integer does not" },
      {  29, "subprogram body is not allowed in package specification" },
      {  36, "unit WORK.BAD not found in library WORK" },
      {  48, "name A cannot be used in this context" },
      {  51, "function arguments must have mode IN" },
      {  56, "function must contain a return statement" },
      {  62, "duplicate declaration of function FOO" },
      { 114, "positional parameters must precede named parameters" },
      { 115, "duplicate parameter name X" },
      { 124, "function arguments may not have VARIABLE class" },
      { 146, "object X has class CONSTANT and cannot be associated" },
      { 161, "pure function WORK.FUNC.TEST18 cannot call impure function" },
      { 166, "object X with access type must have class VARIABLE" },
      { 180, "no suitable overload for function TEST20" },
      { 181, "missing actual for formal Y without default value" },
      { 182, "no suitable overload for function TEST20" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_array)
{
   tree_t p, a, e;

   input_from_file(TESTDIR "/sem/array.vhd");

   const error_t expect[] = {
      { 27, "positional associations must appear first in aggregate" },
      { 33, "named association must not follow others" },
      { 39, "only a single others association allowed" },
      { 46, "type of initial value universal integer does not match" },
      { 55, "type of value universal integer does not match type of" },
      { 57, "type of value INT_ARRAY does not match type" },
      { 65, "for operator \"=\"(INT_ARRAY, TEN_INTS)" },
      { 88, "array W has 2 dimensions but 1 indices given" },
      { 89, "array W has 2 dimensions but 3 indices given" },
      { 98, "type of index universal integer does not match type" },
      { 102, "named and positional associations cannot be mixed in" },
      { 111, "non-locally static choice must be only choice" },
      { 119, "type of slice prefix is not an array" },
      { 120, "range direction of slice does not match prefix" },
      { 121, "others choice not allowed in this context" },
      { 130, "range direction of slice does not match prefix" },
      { 207, "array BAD cannot have unconstrained element type" },
      { 214, "aggregate size mismatch in dimension 1" },
      { 215, "OTHERS choice not allowed in unconstrained" },
      { 232, "aliased name is not static" },
      { 233, "aliased name is not static" },
      { 234, "type of aliased object INT_ARRAY does not match" },
      { 241, "undefined identifier I" },
      { 246, "universal integer bound must be numeric literal or attribute" },
      { 252, "expected 1 constraints for type INT_ARRAY but found 2" },
      { 272, "no suitable overload for operator \"<\"(INT2D, INT2D)" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_assert)
{
   tree_t a, e;

   input_from_file(TESTDIR "/sem/assert.vhd");

   const error_t expect[] = {
      { 17, "type of assertion expression must be BOOLEAN" },
      { 22, "type of message be STRING" },
      { 27, "type of severity must be SEVERITY_LEVEL" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_generics)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/generics.vhd");

   const error_t expect[] = {
      {  34, "missing actual for formal N" },
      {  38, "too many positional actuals" },
      {  48, "undefined identifier X" },
      {  58, "invalid object class for generic" },
      {  68, "undefined identifier Y" },
      { 104, "actual must be globally static expression or locally " },
      { -1, NULL }
   };
   expect_errors(expect);

   // Entity bot

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   // Entity top

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   // Entity bad

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   // Entity class

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   // Package p

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   // Entity static

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_seq)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/seq.vhd");

   const error_t expect[] = {
      {  15, "type of test must be BOOLEAN" },
      {  19, "undefined identifier X" },
      {  25, "name TRUE cannot be used in this context" },
      {  32, "undefined identifier X" },
      {  48, "return statement not allowed outside subprogram" },
      {  62, "return statement not allowed outside subprogram" },
      {  64, "type of loop condition must be BOOLEAN" },
      {  79, "undefined identifier X" },
      { 106, "others choice must appear last" },
      { 113, "case choice must be locally static" },
      { 126, "case choice must be locally static" },
      { 136, "case choice must be locally static" },
      { 139, "missing choice C in case statement" },
      { 145, "invalid use of BIT" },
      { 152, "type mismatch in range" },
      { 156, "case choice range must have type INTEGER" },
      { 160, "right index of case choice range is not locally static" },
      { 170, "type of exit condition must be BOOLEAN" },
      { 185, "cannot associate this expression with parameter" },
      { 193, "type of next condition must be BOOLEAN" },
      { 196, "cannot use next outside loop" },
      { 198, "no nested loop with label FOO" },
      { 211, "duplicate statement label DUP" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_conc)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/conc.vhd");

   const error_t expect[] = {
      { 12, "name '4' cannot be used in this context" },
      { 16, "type of condition must be BOOLEAN" },
      { 18, "reject interval must have type TIME" },
      { 26, "choice must be locally static" },
      { 29, "name TRUE cannot be used in this context" },
      { 32, "name FALSE cannot be used in this context" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_procedure)
{
   tree_t p;

   input_from_file(TESTDIR "/sem/procedure.vhd");

   const error_t expect[] = {
      {   5, "subprogram body is not allowed in package specification" },
      {  28, "cannot return a value from a procedure" },
      {  45, "type of default value universal integer does not match" },
      {  63, "no matching procedure DIFF_TYPES" },
      {  64, "positional parameters must precede named parameters" },
      {  83, "cannot read output port Y" },
      {  84, "invalid target of variable assignment" },
      {  90, "implicit signal STABLE cannot be used in a subprogram body" },
      {  91, "implicit signal QUIET cannot be used in a subprogram body" },
      {  92, "implicit signal TRANSACTION cannot be used in a subprogram" },
      {  93, "implicit signal DELAYED cannot be used in a subprogram" },
      {  98, "object X with access type must have class VARIABLE" },
      {  99, "object X with access type must have class VARIABLE" },
      { 100, "object X with access type must have class VARIABLE" },
      { 137, "sorry, this form of parameter name is not yet supported" },
      { 142, "cannot read output port X" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   fail_if(sem_check(p));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_concat)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/concat.vhd");

   const error_t expect[] = {
      { 24, "type of scalar does not match element type of array" },
      { 26, "cannot concatenate values of different types" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_conv)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/conv.vhd");

   const error_t expect[] = {
      { 28, "conversion only allowed between closely related types" },
      { 29, "type of value B does not match type of target A" },
      { 31, "conversion only allowed between closely related types" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_attr)
{
   tree_t a, e;

   input_from_file(TESTDIR "/sem/attr.vhd");

   const error_t expect[] = {
      { 30, "Z has no attribute FOO" },
      { 52, "invalid attribute reference" },
      { 54, "prefix of user defined attribute reference cannot denote" },
      { 65, "expected attribute type INTEGER" },
      { 66, "expected attribute type STRING" },
      { 67, "undefined identifier Q" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_generate)
{
   tree_t a, e;

   input_from_file(TESTDIR "/sem/generate.vhd");

   const error_t expect[] = {
      { 15, "condition of generate statement must be BOOLEAN" },
      { 26, "undefined identifier Y" },
      { 45, "condition of generate statement must be static" },
      { 48, "range of generate statement must be static" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   fail_if(sem_check(a));

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_record)
{
   tree_t p, d;
   type_t t;

   input_from_file(TESTDIR "/sem/record.vhd");

   const error_t expect[] = {
      {  9, "duplicate field name X" },
      { 15, "recursive record types are not allowed" },
      { 30, "field X with unconstrained array type is not allowed" },
      { 39, "field Z does not have a value" },
      { 40, "does not match type of field Y" },
      { 42, "field Y does not have a value" },
      { 43, "type R1 does not have field named Q" },
      { 44, "type of value R1 does not match type of" },
      { 47, "field X already has a value" },
      { 48, "field X already has a value" },
      { 64, "type R1_VEC is unconstrained" },
      { 72, "record type R1 has no field F" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_file)
{
   tree_t p, d;
   type_t t;

   input_from_file(TESTDIR "/sem/file.vhd");

   const error_t expect[] = {
      {  6, "files may not be of access type" },
      {  8, "files may not be of file type" },
      { 12, "file declarations must have file type" },
      { 16, "open mode must have type FILE_OPEN_KIND" },
      { 20, "file name must have type STRING" },
      { 36, "no suitable overload for procedure READ" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_access)
{
   tree_t p, d;
   type_t t;

   input_from_file(TESTDIR "/sem/access.vhd");

   const error_t expect[] = {
      {  5, "type FOO is not defined" },
      { 34, "null expression must have access type" },
      { 38, "invalid allocator expression" },
      { 39, "name I does not refer to a type" },
      { 41, "does not match type of target INT_PTR" },
      { 47, "type of value REC does not match type of" },
      { 55, "type of allocator expresion INTEGER does not match" },
      { 56, "name S does not refer to a type" },
      { -1, NULL }
   };
   expect_errors(expect);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACK_BODY);
   sem_check(p);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_real)
{
   tree_t a, e;

   input_from_file(TESTDIR "/sem/real.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   const error_t expect[] = {
      { 16, "type of value MY_REAL does not match type of target" },
      { 25, "conversion only allowed between closely related types" },
      { -1, NULL }
   };
   expect_errors(expect);

   sem_check(a);
   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_entity)
{
   tree_t a, e, p;

   input_from_file(TESTDIR "/sem/entity.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   const error_t expect[] = {
      { 22, "unit WORK.PACK is not an entity" },
      { -1, NULL }
   };
   expect_errors(expect);

   sem_check(a);
   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

START_TEST(test_signal)
{
   tree_t a, e;

   input_from_file(TESTDIR "/sem/signal.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   const error_t expect[] = {
      { 14, "no composite type in context" },
      { 15, "no composite type in context" },
      { 16, "not a suitable l-value" },
      { 17, "others association not allowed in aggregate signal target" },
      { 18, "cannot assign to input port P" },
      { 22, "no composite type in context" },
      { 23, "not a suitable l-value" },
      { 24, "others association not allowed in aggregate signal target" },
      { 25, "cannot assign to input port P" },
      { -1, NULL }
   };
   expect_errors(expect);

   sem_check(a);
   fail_unless(sem_errors() == (sizeof(expect) / sizeof(error_t)) - 1);
}
END_TEST

int main(void)
{
   register_trace_signal_handlers();

   setenv("NVC_LIBPATH", "../lib/std", 1);

   Suite *s = suite_create("sem");

   TCase *tc_core = tcase_create("Core");
   tcase_add_unchecked_fixture(tc_core, setup, teardown);
   tcase_add_test(tc_core, test_integer);
   tcase_add_test(tc_core, test_ports);
   tcase_add_test(tc_core, test_scope);
   tcase_add_test(tc_core, test_ambiguous);
   tcase_add_test(tc_core, test_const);
   tcase_add_test(tc_core, test_std);
   tcase_add_test(tc_core, test_wait);
   tcase_add_test(tc_core, test_func);
   tcase_add_test(tc_core, test_array);
   tcase_add_test(tc_core, test_assert);
   tcase_add_test(tc_core, test_generics);
   tcase_add_test(tc_core, test_seq);
   tcase_add_test(tc_core, test_conc);
   tcase_add_test(tc_core, test_procedure);
   tcase_add_test(tc_core, test_concat);
   tcase_add_test(tc_core, test_conv);
   tcase_add_test(tc_core, test_attr);
   tcase_add_test(tc_core, test_generate);
   tcase_add_test(tc_core, test_record);
   tcase_add_test(tc_core, test_file);
   tcase_add_test(tc_core, test_access);
   tcase_add_test(tc_core, test_real);
   tcase_add_test(tc_core, test_entity);
   tcase_add_test(tc_core, test_signal);
   suite_add_tcase(s, tc_core);

   SRunner *sr = srunner_create(s);
   srunner_run_all(sr, CK_NORMAL);

   int nfail = srunner_ntests_failed(sr);

   srunner_free(sr);

   return nfail == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
