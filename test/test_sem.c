#include "type.h"
#include "phase.h"
#include "util.h"
#include "common.h"
#include "test_util.h"
#include "loc.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   tree_t a, d, p, s, e;
   type_t t;

   const error_t expect[] = {
      { 35, "no visible declaration for NOTHING" },
      { 48, "no matching operator \"+\" [ANOTHER_ONE, universal_integer "
        "return MY_INT1]" },
      { 48, "no matching operator \"+\" [MY_INT1, universal_integer "
        "return ANOTHER_ONE]" },
      { 57, "MY_INT2 has no attribute CAKE" },
      { 20, "MY_INT1 does not match type of target MY_INT2" },
      { 30, "MY_INT1 does not match type of target MY_INT2_SUB" },
      { 61, "right bound must be of some integer type but have universal" },
      { 63, "range bounds must be of some integer type but have BOOLEAN" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/sem/integer.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   sem_check(a);
   check_expected_errors();

   d = search_decls(a, ident_new("X"), 0);
   t = tree_type(d);
   fail_unless(type_kind(t) == T_INTEGER);
   e = tree_value(d);
   fail_unless(tree_kind(e) == T_LITERAL);
   t = tree_type(e);
   fail_unless(type_kind(t) == T_INTEGER);

   fail_unless(tree_stmts(a) == 7);

   // Process 1

   p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);

   d = search_decls(p, ident_new("Z"), 0);
   fail_unless(type_kind(tree_type(d)) == T_INTEGER);

   s = tree_stmt(p, 0);
   fail_unless(tree_ref(tree_target(s)) == d);

   // Process 6

   p = tree_stmt(a, 4);
   fail_unless(tree_kind(p) == T_PROCESS);
}
END_TEST

START_TEST(test_ports)
{
   opt_set_int("missing-body", 1);

   input_from_file(TESTDIR "/sem/ports.vhd");

   const error_t expect[] = {
      { 31,  "cannot read output port O" },
      { 42,  "cannot assign to input port I" },
      { 92,  "WORK.FOO has no port named CAKE" },
      { 10,  "entity WORK.FOO has ports O, I" },
      { 94,  "cannot find unit WORK.BAD" },
      { 116, "object X is not a component declaration" },
      { 155, "BAR has no port named Q" },
      { 64,  "BAR has ports I, O" },
      { 163, "BAR has no port named U" },
      { 64,  "component BAR has ports I, O" },
      { 185, "no visible subprogram declaration for HELLO" },
      { 81,  "missing actual for port I of mode IN without a default " },
      { 85,  "formal I already has an actual" },
      { 89,  "at least 3 positional actuals but WORK.FOO has only 2 ports" },
      { 103, "unconnected port I with mode IN must have a default value" },
      { 148, "port O of mode OUT must be a static signal name or OPEN" },
      { 168, "formal name must be locally static" },
      { 177, "formal name must be locally static" },
      { 217, "port O of unconstrained type INT_VEC cannot be unconnected" },
      { 221, "type of actual universal_real does not match type INTEGER" },
      { 256, "missing body for function FUNC1 [BIT return MY_INT1]" },
      { 257, "missing body for function FUNC2 [BIT, INTEGER return MY_INT1]" },
      { 258, "missing body for function FUNC3 [BIT return INTEGER]" },
      { 259, "missing body for function FUNC4 [INTEGER return BIT]" },
      { 272, "result of conversion for unconstrained formal I must" },
      { 280, "port N of mode IN must be a globally static expression or " },
      { 284, "conversion not allowed for formal O with mode OUT" },
      { 294, "output conversion not allowed for formal I with mode IN" },
      { 298, "output conversion for formal B must not have OPEN actual" },
      { 304, "port B of mode INOUT must be a static signal name or OPEN" },
      { 307, "port B of mode INOUT must be a static signal name or OPEN" },
      { 318, "cannot assign to input port X" },
      { 319, "cannot read output port Y" },
      { 339, "cannot read parameter X with mode IN" },
      { 353, "missing body for function \"not\" [INTEGER return INTEGER]" },
      { 354, "missing body for function FOO" },
      { 363, "associated with port I of mode IN must be a globally static" },
      { 367, "actual associated with port I of mode IN must be a globally " },
      { 375, "invalid output conversion \"not\"" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ARCH,
                   T_ARCH, T_ENTITY, T_ARCH, T_ARCH, T_ARCH);

   fail_unless(parse() == NULL);
   check_expected_errors();
}
END_TEST

START_TEST(test_scope)
{
   input_from_file(TESTDIR "/sem/scope.vhd");

   const error_t expect[] = {
      {  31, "WORK.PACK1.MY_INT1 does not match type"
         " of target WORK.PACK2.MY_INT1" },
      {  44, "WORK.PACK1.MY_INT1 does not match type of target "
         "WORK.NO_USE_CLAUSE-A.MY_INT1" },
      {  63, "G already declared in this region" },
      {  54, "previous declaration of G was here" },
      {  71, "P already declared in this region" },
      {  55, "previous declaration of P was here" },
      { 114, "no visible declaration for MY_INT1" },
      { 137, "no visible declaration for E1" },
      { 160, "no visible subprogram declaration for FUNC2" },
      { 167, "object NOT_HERE not found in unit WORK.PACK5" },
      { 189, "no visible declaration for MY_INT1" },
      { 236, "no visible declaration for FOO" },
      { 306, "name X not found in L1" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY,
                   T_ARCH, T_ARCH, T_ARCH, T_ENTITY, T_ARCH, T_ENTITY,
                   T_PACKAGE, T_PACKAGE, T_ARCH, T_PACKAGE, T_ARCH,
                   T_ARCH, T_ENTITY, T_ARCH, T_ARCH, T_PACKAGE, T_ARCH,
                   T_ARCH, T_PACKAGE, T_ARCH, T_ARCH, T_PACKAGE, T_ARCH,
                   T_ARCH, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_ambiguous)
{
   tree_t a, e, p, s;
   type_t lhs, rhs;

   input_from_file(TESTDIR "/sem/ambiguous.vhd");

   const error_t expect[] = {
      {  56, "type of aggregate is ambiguous" },
      {  56, "type of aggregate is ambiguous" },
      {  86, "ambiguous use of enumeration literal FALSE" },
      {  24, "visible declaration of FALSE as BOOLEAN" },
      {  84, "visible declaration of FALSE as T" },
      {  93, "ambiguous call to function NOW" },
      {  91, "candidate NOW [return INTEGER]" },
      {  94, "candidate NOW [return DELAY_LENGTH]" },
      { 103, "ambiguous use of name FALSE (FALSE [return INTEGER], BOOLEAN)" },
      {  24, "visible declaration of FALSE as BOOLEAN" },
      {  98, "visible declaration of FALSE as FALSE [return INTEGER]" },
      {  35, "type of value BAR does not match type of target FOO" },
      { 141, "ambiguous use of operator \"<\"" },
      { 127, "candidate \"<\" [MY_INT, MY_INT return BOOLEAN]" },
      { 121, "candidate \"<\" [MY_INT, MY_INT return BOOLEAN]" },
      { 222, "type of aggregate is ambiguous (T_VEC, STRING, BIT_VECTOR)" },
      { 222, "type of aggregate is ambiguous (T_VEC, STRING, BIT_VECTOR)" },
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
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.FOO"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.FOO"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.BAR"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.BAR"));

   p = tree_stmt(a, 1);
   fail_unless(tree_stmts(p) == 2);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.FOO"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.FOO"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.BAR"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.BAR"));

   p = tree_stmt(a, 2);
   fail_unless(tree_stmts(p) == 3);
   s = tree_stmt(p, 0);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.P3.BAZ"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.P3.BAZ"));
   s = tree_stmt(p, 1);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(s));
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.P3.BAZ"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.P3.BAZ"));
   s = tree_stmt(p, 2);
   lhs = tree_type(tree_target(s));
   rhs = tree_type(tree_value(tree_waveform(s, 0)));
   fail_unless(type_ident(lhs) == ident_new("WORK.E-A.FOO"));
   fail_unless(type_ident(rhs) == ident_new("WORK.E-A.FOO"));

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ARCH, T_ARCH, T_ARCH,
                   T_ARCH, T_ARCH, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_const)
{
   input_from_file(TESTDIR "/sem/const.vhd");

   const error_t expect[] = {
      { 24, "target of variable assignment must be a variable name" },
      { 28, "deferred constant declarations are only permitted" },
      { 58, "C already declared in this region" },
      { 57, "previous declaration of C was here" },
      { 59, "expected type INTEGER for deferred constant F" },
      { 49, "deferred constant D was not given a value" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_const2)
{
   input_from_file(TESTDIR "/sem/const2.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   fail_unless(error_count() == 0);

   fail_unless(tree_kind(p) == T_PACKAGE);

   tree_t d = search_decls(p, ident_new("DEF_ARR"), 0);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(type_is_unconstrained(tree_type(d)));
   fail_if(tree_has_value(d));

   tree_t pb = parse();
   fail_if(pb == NULL);
   fail_unless(tree_kind(pb) == T_PACK_BODY);
   sem_check(pb);

   fail_if_errors();
}
END_TEST

START_TEST(test_std)
{
   input_from_file(TESTDIR "/sem/std.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_wait)
{
   input_from_file(TESTDIR "/sem/wait.vhd");

   const error_t expect[] = {
      { 35, "invalid use of entity A" },
      { 17, "type of delay must be TIME" },
      { 26, "name V in sensitivity list is not a signal" },
      { 40, "wait statement not allowed in process" },
      { 51, "type of condition must be BOOLEAN" },
      { 53, "type of delay must be TIME" },
      { 66, "name in sensitivity list is not static" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   sem_check(e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);
   sem_check(a);

   tree_t p = tree_stmt(a, 0);
   fail_unless(tree_kind(p) == T_PROCESS);
   fail_unless(tree_stmts(p) == 4);

   ident_t time = ident_new("STD.STANDARD.TIME");

   tree_t w = tree_stmt(p, 0);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   w = tree_stmt(p, 1);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   w = tree_stmt(p, 2);
   fail_unless(tree_kind(w) == T_WAIT);
   fail_unless(type_ident(tree_type(tree_delay(w))) == time);

   fail_unless(parse() == NULL);

   check_expected_errors();
}
END_TEST

START_TEST(test_func)
{
   input_from_file(TESTDIR "/sem/func.vhd");

   const error_t expect[] = {
      {  25, "no visible subprogram declaration for UENUM" },
      {   5, "function arguments must have mode IN" },
      {  19, "must be an unconstrained array type" },
      {  23, "resolution function must have a single argument" },
      {  27, "type of default value universal_integer does not" },
      {  29, "subprogram body is not allowed in package specification" },
      {  36, "missing declaration for package WORK.BAD" },
      {  62, "FOO [INTEGER, INTEGER, INTEGER return INTEGER] already " },
      {  61, "previous declaration of FOO [INTEGER, INTEGER, INTEGER" },
      {  48, "expected return type INTEGER but have UENUM" },
      {  51, "function arguments must have mode IN" },
      {  56, "function must contain a return statement" },
      { 114, "positional parameters must precede named parameters" },
      { 115, "duplicate parameter name X" },
      { 124, "function arguments may not have VARIABLE class" },
      { 146, "object X has class CONSTANT and cannot be associated" },
      { 161, "pure function TEST18 cannot call impure function" },
      { 166, "object X with access type must have class VARIABLE" },
      { 180, "universal_real does not match formal X type INTEGER" },
      { 181, "missing actual for formal Y without default value" },
      { 182, "type of actual universal_integer does not match formal Y" },
      { 239, "class variable of subprogram body TEST25 parameter" },
      { 234, "parameter X was originally declared here" },
      { 245, "class constant of subprogram body TEST26 parameter" },
      { 243, "parameter X was originally declared here" },
      { 271, "invalid reference to X inside pure function NESTED" },
      { 288, "no visible subprogram declaration for FNORK" },
      { 294, "procedure NOTDEF not allowed in an expression" },
      { 297, "no visible declaration for BAD_TYPE" },
      { 297, "no visible declaration for FOO" },
      { 293, "function CONSTPURE cannot be called as a procedure" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACK_BODY, T_PACKAGE,
                   T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_array)
{
   input_from_file(TESTDIR "/sem/array.vhd");

   const error_t expect[] = {
      { 65,  "operator \"=\" [INT_ARRAY, TEN_INTS return BOOLEAN]" },
      { 119, "aggregate has non-composite type INTEGER" },
      { 272, "no matching operator \"<\" [INT2D, INT2D" },
      { 277, "no visible declaration for NOT_HERE" },
      { 424, "ambiguous call to function F" },
      { 418, "candidate F [INTEGER return STRING]" },
      { 419, "candidate F [return STRING]" },
      { 27,  "positional associations must appear first in aggregate" },
      { 33,  "named association must not follow others" },
      { 39,  "only a single others association allowed" },
      { 46,  "type of initial value universal_integer does not match" },
      { 55,  "type of value universal_integer does not match type of" },
      { 57,  "type of value INT_ARRAY does not match type" },
      { 88,  "array W has 2 dimensions but 1 indices given" },
      { 89,  "array W has 2 dimensions but 3 indices given" },
      { 98,  "type of index universal_integer does not match type" },
      { 102, "named and positional associations cannot be mixed in" },
      { 111, "a choice that is not locally static is allowed" },
      { 119, "type of slice prefix is not an array" },
      { 120, "range direction of slice TO does not match prefix DOWNTO" },
      { 121, "index range of array aggregate with others choice cannot" },
      { 130, "range direction of slice DOWNTO does not match prefix TO" },
      { 207, "array BAD cannot have unconstrained element type" },
      { 215, "array aggregate with others choice cannot be determined" },
      { 232, "aliased name is not static" },
      { 233, "aliased name is not static" },
      { 234, "type of aliased object INT_ARRAY does not match" },
      { 241, "cannot index non-array type INTEGER" },
      { 252, "expected 1 constraints for type INT_ARRAY but found 2" },
      { 279, "type NUM_ARRAY is unconstrained" },
      { 285, "object K does not have a range" },
      { 295, "type of index universal_integer does not match" },
      { 343, "invalid character 'f' in string literal of type BIT_VECTOR" },
      { 365, "may not change constraints of constrained array type TEN_INTS" },
      { 366, "may not change constraints of constrained array type TEN_INTS" },
      { 379, "array T_FILE_ARRAY cannot have element of file type" },
      { 391, "index type REAL is not discrete" },
      { 392, "index type BIT_VECTOR is not discrete" },
      { 399, "type of array aggregate choice BOOLEAN does not match INT_" },
      { 400, "in range: left is universal_integer, right is BOOLEAN" },
      { 401, "expected type of range bounds to be INTEGER but have BOOLEAN" },
      { 403, "a choice that is not locally static is allowed" },
      { 404, "a choice that is not locally static is allowed" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_assert)
{
   input_from_file(TESTDIR "/sem/assert.vhd");

   const error_t expect[] = {
      { 17, "type of assertion expression must be BOOLEAN" },
      { 22, "type of message be STRING" },
      { 27, "type of severity must be SEVERITY_LEVEL" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_generics)
{
   opt_set_int("missing-body", 1);
   input_from_file(TESTDIR "/sem/generics.vhd");

   const error_t expect[] = {
      {  34, "missing actual for generic N" },
      {  38, "2 positional actuals but WORK.BOT has only 1 generic" },
      {  48, "no visible declaration for X" },
      {  58, "invalid object class for generic" },
      {  68, "no visible declaration for Y" },
      {  86, "missing body for function F [BIT_VECTOR return INTEGER]" },
      { 109, "with generic X must be a globally static expression" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ENTITY, T_ENTITY,
                   T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_seq)
{
   input_from_file(TESTDIR "/sem/seq.vhd");

   const error_t expect[] = {
      {  19, "no visible declaration for X" },
      {  33, "no visible declaration for G" },
      {  79, "no visible declaration for X" },
      { 205, "DUP already declared in this region" },
      { 204, "previous declaration of DUP was here" },
      { 228, "expected type mark while parsing discrete range" },
      {  15, "type of condition must be BOOLEAN" },
      {  25, "type of value BOOLEAN does not match type of target INTEGER" },
      {  48, "return statement not allowed outside subprogram" },
      {  62, "return statement not allowed outside subprogram" },
      {  64, "type of loop condition must be BOOLEAN" },
      { 106, "others choice must appear last" },
      { 113, "case choice must be locally static" },
      { 126, "case choice must be locally static" },
      { 136, "case choice must be locally static" },
      { 139, "invalid use of type BIT" },
      { 146, "type mismatch in range" },
      { 150, "expected type of range bounds to be INTEGER but have BIT" },
      { 154, "right index of case choice range is not locally static" },
      { 164, "type of exit condition must be BOOLEAN" },
      { 179, "actual for formal Y with class VARIABLE must be" },
      { 187, "type of next condition must be BOOLEAN" },
      { 190, "cannot use next outside loop" },
      { 192, "no nested loop with label FOO" },
      { 214, "type REAL does not have a range" },
      { 222, "variable I is not a valid target of signal assignment" },
      { 230, "range bounds to be INTEGER but have universal_real" },
      { 232, "type of range bounds REAL is not discrete" },
      { 243, "target of variable assignment must be a variable name" },
      { 244, "aggregate element must be locally static name" },
      { 245, "others association not allowed in aggregate variable target" },
      { 246, "range association not allowed in aggregate variable target" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_conc)
{
   input_from_file(TESTDIR "/sem/conc.vhd");

   const error_t expect[] = {
      { 12, "type of value CHARACTER does not match type of target INTEGER" },
      { 16, "type of condition must be BOOLEAN" },
      { 18, "reject interval must have type TIME" },
      { 26, "choice must be locally static" },
      { 29, "type of value BOOLEAN does not match type of target INTEGER" },
      { 32, "case choice must have type INTEGER" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_procedure)
{
   input_from_file(TESTDIR "/sem/procedure.vhd");

   const error_t expect[] = {
      {   5, "subprogram body is not allowed in package specification" },
      { 180, "invalid procedure call statement" },
      { 186, "?? is a reserved word in VHDL-2008" },
      { LINE_INVALID, "pass --std=2008 to enable this feature" },
      { 186, "unexpected error while parsing primary" },
      {  28, "cannot return a value from a procedure" },
      {  45, "type of default value universal_integer does not match" },
      {  63, "missing actual for formal X without default value" },
      {  64, "positional parameters must precede named parameters" },
      {  83, "cannot read output port Y" },
      {  84, "target of variable assignment must be a variable name" },
      {  90, "implicit signal STABLE cannot be used in a subprogram body" },
      {  91, "implicit signal QUIET cannot be used in a subprogram body" },
      {  92, "implicit signal TRANSACTION cannot be used in a subprogram" },
      {  93, "implicit signal DELAYED cannot be used in a subprogram" },
      {  98, "object X with access type must have class VARIABLE" },
      {  99, "object X with access type must have class VARIABLE" },
      { 100, "object X with access type must have class VARIABLE" },
      { 137, "sorry, this form of parameter name is not yet supported" },
      { 142, "cannot read output port X" },
      { 148, "target of signal assignment is not a signal" },
      { 157, "object ARG with type containing an access type must have class" },
      { 162, "object ARG with type containing an access type must have class" },
      { 167, "object ARG with type containing an access type must have class" },
      { 172, "object ARG with type containing an access type must have class" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_concat)
{
   input_from_file(TESTDIR "/sem/concat.vhd");

   const error_t expect[] = {
      { 24, "no matching operator \"&\" [universal_integer, STRING" },
      { 26, "no matching operator \"&\" [universal_integer, CHARACTER" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_conv)
{
   input_from_file(TESTDIR "/sem/conv.vhd");

   const error_t expect[] = {
      { 28, "conversion only allowed between closely related types" },
      { 29, "type of value B does not match type of target A" },
      { 31, "conversion only allowed between closely related types" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_attr)
{
   input_from_file(TESTDIR "/sem/attr.vhd");

   const error_t expect[] = {
      {  30, "Z has no attribute FOO" },
      {  54, "prefix of user defined attribute reference cannot denote" },
      {  67, "no visible declaration for Q" },
      {  68, "no visible declaration for YAH" },
      {  65, "expected attribute type INTEGER" },
      {  66, "expected attribute type STRING" },
      {  85, "dimension of attribute LEFT must be locally static" },
      { 101, "prefix of SIMPLE_NAME attribute must be a named entity" },
      { 103, "prefix of PATH_NAME attribute must be a named entity" },
      { 139, "class of object I is variable not signal" },
      { 133, "cannot index non-array type INTEGER" },
      { 146, "prefix of attribute LAST_EVENT must denote a signal" },
      { 158, "attribute RANGE with unconstrained array type BIT_VECTOR" },
      { 159, "object B does not have a range" },
      { 160, "prefix does not have a range" },
      { 204, "prefix does not have LENGTH attribute" },
      { 220, "prefix of BASE attribute must be a type or subtype declaration" },
      { 221, "BASE attribute is allowed only as the prefix of the name" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH, T_ARCH, T_PACKAGE, T_PACK_BODY,
                   T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_generate)
{
   input_from_file(TESTDIR "/sem/generate.vhd");

   const error_t expect[] = {
      { 26, "no visible declaration for Y" },
      { 15, "condition of generate statement must be BOOLEAN" },
      { 45, "condition of generate statement must be static" },
      { 48, "range of generate statement must be static" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_record)
{
   input_from_file(TESTDIR "/sem/record.vhd");

   const error_t expect[] = {
      {   9, "duplicate field name X" },
      {  15, "recursive record types are not allowed" },
      {  30, "field X with unconstrained array type is not allowed" },
      {  43, "record type R1 has no field named Q" },
      {  72, "record type R1 has no field named F" },
      {  82, "record type R1_SUB has no field named Z" },
      { 106, "record type R1 has no field named Z" },
      { 124, "record type R8 has no field named ACK" },
      { 170, "record type R1 has no field named Z" },
      {  39, "field Z does not have a value" },
      {  40, "does not match type INTEGER of field Y" },
      {  42, "field Y does not have a value" },
      {  44, "type of value R1 does not match type INTEGER of" },
      {  47, "field X was already given a value by earlier named choice" },
      {  48, "field X was already given a value by earlier positional choice" },
      {  64, "type R1_VEC is unconstrained" },
      {  86, "index constraint cannot be used with non-array type R1" },
      { 111, "record field A cannot be of file type" },
      { 153, "cannot index non-array type UNIT_SPEC_T" },
      { 155, "cannot index non-array type UNIT_SPEC_T" },
      { 166, "association choice must be a field name" },
      { 167, "3 positional associations given but record type R1 only has 2" },
      { 168, "others association must represent at least one element" },
      { 169, "range association invalid in record aggregate" },
      { 170, "range association invalid in record aggregate" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();

   tree_t d = search_decls(p, ident_new("P9"), 0);
   fail_if(d == NULL);
   d = tree_decl(d, 0);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   tree_t v = tree_value(d);
   fail_unless(tree_kind(v) == T_AGGREGATE);
   fail_if(type_is_universal(tree_type(tree_value(tree_assoc(v, 0)))));
}
END_TEST

START_TEST(test_file)
{
   input_from_file(TESTDIR "/sem/file.vhd");

   const error_t expect[] = {
      {  6, "files may not be of access type" },
      {  8, "files may not be of file type" },
      { 12, "file declarations must have file type" },
      { 16, "open mode must have type FILE_OPEN_KIND" },
      { 20, "file name must have type STRING" },
      { 28, "array type for file type must be one-dimensional" },
      { 30, "array type for file type must be one-dimensional" },
      { 46, "type T_PTR_ARR has a subelement with an access type" },
      { 47, "type SUB_PTR_ARR has a subelement with an access type" },
      { 48, "type T_REC has a subelement with an access type" },
      { 49, "type T_REC2 has a subelement with an access type" },
      { 60, "function result subtype may not denote a file type" },
      { 76, "no matching subprogram READ [FT, FILE_OPEN_STATUS]" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_access)
{
   input_from_file(TESTDIR "/sem/access.vhd");

   const error_t expect[] = {
      {  5, "no visible declaration for FOO" },
      { 38, "unexpected integer while parsing type mark, expecting" },
      { 39, "type mark I does not refer to a type" },
      { 56, "type mark S does not refer to a type" },
      { 97, "cannot determine type of allocator expression from context" },
      { 34, "null expression must have access type" },
      { 41, "does not match type of target INT_PTR" },
      { 47, "type of value REC does not match type of" },
      { 55, "type of allocator expresion INTEGER does not match" },
      { 76, "unconstrained array type INT_PTR_ARRAY not allowed" },
      { 84, "index constraint cannot be used with non-array type INTEGER" },
      { 90, "type FOO is incomplete" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_real)
{
   input_from_file(TESTDIR "/sem/real.vhd");

   const error_t expect[] = {
      { 16, "type of value MY_REAL does not match type of target" },
      { 25, "conversion only allowed between closely related types" },
      { 38, "type of left bound must be of some real type but have INTEGER" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_entity)
{
   input_from_file(TESTDIR "/sem/entity.vhd");

   const error_t expect[] = {
      { 23, "cannot find unit WORK.E-INVALID" },
      { 26, "unit WORK.PACK cannot be instantiated" },
      { 30, "unit WORK.PACK is not an entity" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_PACKAGE, T_ARCH, T_ARCH, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_signal)
{
   input_from_file(TESTDIR "/sem/signal.vhd");

   const error_t expect[] = {
      { 14, "has non-composite type BIT" },
      { 15, "type of string literal cannot be determined from context" },
      { 22, "aggregate has non-composite type BIT" },
      { 16, "target of signal assignment must be a signal name" },
      { 17, "others association not allowed in aggregate signal target" },
      { 18, "cannot assign to input port P" },
      { 23, "target of signal assignment must be a signal name" },
      { 24, "others association not allowed in aggregate signal target" },
      { 25, "cannot assign to input port P" },
      { 30, "aggregate element must be locally static name" },
      { 40, "signal X is not a formal parameter and procedure PROC1 [BIT] "
        "is not contained within a process statement" },
      { 48, "implicit signal may not be assigned" },
      { 54, "guard signal must have BOOLEAN type but found INTEGER" },
      { 57, "guard expression must have type BOOLEAN but found BIT_VECTOR" },
      { 64, "assignment guard must be a signal" },
      { 65, "null waveform element is only valid when the target is" },
      { 69, "guarded signal must have resolved subtype" },
      { 78, "disconnection specification must denote a guarded signal" },
      { 79, "disconnection specification must denote a guarded signal" },
      { 80, "type of declared signal RBIT does not match type BIT_VECTOR in" },
      { 81, "disconnection specification must have type TIME but found RBIT" },
      { 83, "time expression in disconnection specificiation must be static" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_static)
{
   input_from_file(TESTDIR "/sem/static.vhd");

   const error_t expect[] = {
      {  36, "case choice must be locally static" },
      {  42, "case choice must be locally static" },
      { 104, "no visible subprogram declaration for BAD_FUNC" },
      {  65, "with port X of mode IN must be a globally static" },
      {  85, "formal name must be locally static" },
      {  -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   tree_t p1 = tree_stmt(a, 5);
   fail_unless(tree_kind(p1) == T_PROCESS);
   fail_unless(tree_ident(p1) == ident_new("P1"));

   tree_t tvalue = tree_value(tree_decl(p1, 0));
   fail_unless(type_eq(tree_type(tvalue), std_type(NULL, STD_TIME)));
   fail_unless(tree_kind(tvalue) == T_FCALL);
   fail_if(tree_flags(tvalue) & TREE_F_LOCALLY_STATIC);
   fail_unless(tree_flags(tvalue) & TREE_F_GLOBALLY_STATIC);

   check_expected_errors();
}
END_TEST

START_TEST(test_subtype)
{
   input_from_file(TESTDIR "/sem/subtype.vhd");

   const error_t expect[] = {
      { 16, "no visible subprogram declaration for NOT_HERE" },
      {  9, "expected type of range bound to be INTEGER but is" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse_and_check(T_PACKAGE);

   tree_t d = search_decls(p, ident_new("BIT_VECTOR_ARRAY"), 0);
   fail_if(d == NULL);

   type_t type = type_elem(tree_type(d));
   fail_unless(type_kind(type) == T_SUBTYPE);

   tree_t right = tree_right(range_of(type, 0));
   fail_if(type_is_universal(tree_type(right)));

   check_expected_errors();
}
END_TEST

START_TEST(test_universal)
{
   input_from_file(TESTDIR "/sem/universal.vhd");

   const error_t expect[] = {
      { 12, "operator \"*\" [REAL, universal_integer return REAL]" },
      { 14, "operator \"*\" [INTEGER, universal_real return REAL]" },
      { 16, "operator \"/\" [universal_real, INTEGER return REAL]" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue52)
{
   input_from_file(TESTDIR "/sem/issue52.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue58)
{
   input_from_file(TESTDIR "/sem/issue58.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_spec)
{
   input_from_file(TESTDIR "/sem/spec.vhd");

   const error_t expect[] = {
      { 24, "E does not name a component" },
      { 34, "cannot find unit WORK.NOT_HERE" },
      { 36, "unit WORK.P cannot be instantiated" },
      { 28, "instance BAD not found" },
      { 32, "instance I1 is already bound by a specification" },
      { 22, "originally bound by specification here" },
      { 36, "instance I3 is already bound by a specification" },
      { 34, "originally bound by specification here" },
      { 40, "instance I5 not found" },
      { 42, "instance I4 is already bound by a specification" },
      { 38, "originally bound by specification here" },
      { 79, "component mismatch for instance I1: expected C1" },
      { 81, "specification may only be used with component instances" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_PACKAGE, T_ENTITY, T_ARCH, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue53)
{
   input_from_file(TESTDIR "/sem/issue53.vhd");

   parse_and_check(T_ENTITY);

   fail_if_errors();
}
END_TEST

START_TEST(test_supersede)
{
   input_from_file(TESTDIR "/sem/supersede.vhd");

   const error_t expect[] = {
      { 18, "WORK.PORTLISTTEST has no formal A" },
      { 19, "WORK.PORTLISTTEST has no formal B" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_implicit)
{
   input_from_file(TESTDIR "/sem/implicit.vhd");

   const error_t expect[] = {
      { 12, "attribute DELAYED parameter must have type TIME" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_config)
{
   input_from_file(TESTDIR "/sem/config.vhd");

   const error_t expect[] = {
      { 42, "no visible declaration for O" },
      { 48, "instance BAD not found" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ENTITY,
                   T_ARCH, T_CONFIGURATION);

   check_expected_errors();
}
END_TEST

START_TEST(test_protected)
{
   opt_set_int("missing-body", 1);
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/protected.vhd");

   const error_t expect[] = {
      {  13, "no visible declaration for NOT_HERE" },
      {  19, "no visible declaration for BAD2" },
      {  22, "object BOOLEAN is not a protected type declaration" },
      {  25, "object NOW is not a protected type declaration" },
      {  47, "SHAREDCOUNTER already declared in this region" },
      {  28, "previous declaration of SHAREDCOUNTER was here" },
      {  50, "subtypes may not have protected base types" },
      {  52, "shared variable X must have protected type" },
      {  56, "variable Z with protected type may not have an initial value" },
      {  58, "function result subtype may not denote a protected type" },
      {  64, "parameter with protected type cannot have a default value" },
      { 118, "no visible subprogram declaration for COUNTER" },
      { 119, "expected 1 argument for subprogram DECREMENT [INTEGER] but " },
      { 124, "object X with protected type must have class VARIABLE" },
      { 135, "may not assign to variable of a protected type" },
      { 150, "missing body for protected type PROTECTED_T" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH, T_PACKAGE, T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_protected2)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/protected2.vhd");

   const error_t expect[] = {
      {  5, "constants may not have protected type" },
      {  5, "deferred constant C was not given a value in the package body" },
      { 20, "files may not be of protected type" },
      { 22, "array T_PROTECTED_ARRAY cannot have element of protected type" },
      { 26, "record field B cannot be of protected type" },
      { 30, "signals may not have protected type" },
      { 31, "attributes may not have protected type" },
      { 35, "generics may not have protected type" },
      { 41, "ports may not have protected type" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_alias)
{
   input_from_file(TESTDIR "/sem/alias.vhd");

   const error_t expect[] = {
      { 10, "non-object alias may not have subtype indication" },
      { 12, "type mark AX does not refer to a type" },
      { 22, "no visible subprogram FOO matches signature [INTEGER "
        "return INTEGER]" },
      { 23, "no visible subprogram FOO matches signature [BIT]" },
      { 24, "invalid name in subprogram alias" },
      { 25, "no visible declaration for BLAH" },
      { 32, "no visible subprogram BAR matches signature [INTEGER]" },
      { 40, "ambiguous use of enumeration literal '1'" },
      { 26, "visible declaration of '1' as BIT" },
      { 36, "visible declaration of '1' as CHARACTER" },
      { 41, "no visible subprogram declaration for FOO_INT" },
      { 68, "unexpected identifier while parsing subtype declaration" },
      { 42, "type of actual CHARACTER does not match formal X type BIT" },
      { 43, "operand of qualified expression must have type CHARACTER" },
      { 50, "aliased name is not static" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t arch = parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();

   tree_t x_decl = tree_decl(arch, 1);
   fail_unless(tree_kind(x_decl) == T_SIGNAL_DECL);
   fail_unless(icmp(tree_ident(x_decl), "X"));
}
END_TEST

START_TEST(test_issue102)
{
   input_from_file(TESTDIR "/sem/issue102.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue105)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/issue105.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue105a)
{
   set_standard(STD_93);

   input_from_file(TESTDIR "/sem/issue105.vhd");

   const error_t expect[] = {
      { 12, "case choice must be locally static" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue88)
{
   input_from_file(TESTDIR "/sem/issue88.vhd");

   const error_t expect[] = {
      { 31, "record type REC2 has no field named P" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue128)
{
   input_from_file(TESTDIR "/sem/issue128.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue130)
{
   input_from_file(TESTDIR "/sem/issue130.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue132)
{
   input_from_file(TESTDIR "/sem/issue132.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue131)
{
   input_from_file(TESTDIR "/sem/issue131.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue133)
{
   input_from_file(TESTDIR "/sem/issue133.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue140)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/sem/issue140.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH,
                   T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue144)
{
   input_from_file(TESTDIR "/sem/issue144.vhd");

   const error_t expect[] = {
      { 11, "duplicate subprogram body FUN [return INTEGER]" },
      {  6, "previous definition of FUN [return INTEGER] was here" },
      { 20, "duplicate subprogram body PROC [INTEGER]" },
      { 16, "previous definition of PROC [INTEGER] was here" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue151)
{
   input_from_file(TESTDIR "/sem/issue151.vhd");

   parse_and_check(T_PACKAGE, T_PACKAGE, T_PACKAGE);

   fail_if_errors();
}
END_TEST

START_TEST(test_duplicate)
{
   input_from_file(TESTDIR "/sem/duplicate.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue165)
{
   input_from_file(TESTDIR "/sem/issue165.vhd");

   const error_t expect[] = {
      {  5, "no visible declaration for TYPE_T" },
      { 11, "no visible subprogram declaration for PROC" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue162)
{
   input_from_file(TESTDIR "/sem/issue162.vhd");

   tree_t body = parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();

   fail_unless(tree_kind(body) == T_PACK_BODY);

   tree_t p = tree_decl(body, 2);
   fail_unless(tree_kind(p) == T_PROC_BODY);
   fail_unless(icmp(tree_ident(p), "CALLING_PROC"));
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 0)))->first_line == 10);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 1)))->first_line == 10);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 2)))->first_line == 5);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 3)))->first_line == 5);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 4)))->first_line == 10);

   tree_t f1 = tree_decl(body, 5);
   fail_unless(tree_kind(f1) == T_FUNC_BODY);
   fail_unless(icmp(tree_ident(f1), "CALLING_FUN_WORKS"));

   tree_t c0 = tree_value(tree_param(tree_value(tree_stmt(f1, 0)), 0));
   fail_unless(tree_loc(tree_ref(c0))->first_line == 29);

   tree_t c1 = tree_value(tree_param(tree_value(tree_stmt(f1, 1)), 0));
   fail_unless(tree_loc(tree_ref(c1))->first_line == 23);

   tree_t c2 = tree_value(tree_param(tree_value(tree_stmt(f1, 2)), 0));
   fail_unless(tree_loc(tree_ref(c2))->first_line == 23);

   tree_t f2 = tree_decl(body, 6);
   fail_unless(tree_kind(f2) == T_FUNC_BODY);
   fail_unless(icmp(tree_ident(f2), "CALLING_FUN"));

   tree_t c3 = tree_value(tree_stmt(f2, 0));
   fail_unless(tree_loc(tree_ref(c3))->first_line == 29);
}
END_TEST

START_TEST(test_issue178)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/issue178.vhd");

   const error_t expect[] = {
      { 13, "wait statement not allowed in protected subprogram body" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue177)
{
   input_from_file(TESTDIR "/sem/issue177.vhd");

   const error_t expect[] = {
      {  9, "wait statement not allowed in function body" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_use)
{
   input_from_file(TESTDIR "/sem/use.vhd");

   const error_t expect[] = {
      { 25, "no visible declaration for MY_INT3" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_PACK_BODY, T_ENTITY);

   check_expected_errors();
}
END_TEST

START_TEST(test_afunc)
{
   input_from_file(TESTDIR "/sem/afunc.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_if_errors();

   tree_t f = tree_value(tree_stmt(tree_stmt(a, 0), 0));
   fail_unless(tree_kind(f) == T_FCALL);

   tree_t r = tree_value(tree_param(f, 0));
   fail_unless(tree_kind(r) == T_ARRAY_REF);

   tree_t c = tree_value(r);
   fail_unless(tree_kind(c) == T_FCALL);
   fail_unless(icmp(tree_ident(c), "GET"));
   fail_unless(tree_params(c) == 0);
}
END_TEST

START_TEST(test_issue173)
{
   input_from_file(TESTDIR "/sem/issue173.vhd");

   lib_t other = lib_tmp("lib");
   lib_set_work(other);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue174)
{
   input_from_file(TESTDIR "/sem/issue174.vhd");

   lib_t other = lib_tmp("lib");
   lib_set_work(other);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_varinit)
{
   input_from_file(TESTDIR "/sem/varinit.vhd");

   const error_t expect[] = {
      { 32, "cannot reference signal SIZE during static elaboration" },
      { 43, "cannot reference signal SIZE during static elaboration" },
      { 45, "cannot reference signal SIZE during static elaboration" },
      { 53, "cannot reference signal N during static elaboration" },
      { 54, "cannot reference signal N during static elaboration" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_PACKAGE, T_ARCH, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue201)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/issue201.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue176)
{
   input_from_file(TESTDIR "/sem/issue176.vhd");

   const error_t expect[] = {
      { 36, "function FUN cannot call procedure PROC_WAIT_INDIRECT" },
      { 37, "function FUN cannot call procedure PROC_WAIT which" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_context)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/context.vhd");

   const error_t expect[] = {
      { 27, "unit FOO.PACK is not a context declaration" },
      { 42, "missing declaration for context WORK.BLAH" },
      { 40, "library clause in a context declaration may not have logical" },
      { 41, "context declaration use clause may not have WORK" },
      { 42, "context declaration context reference may not have WORK" },
      { -1, NULL }
   };
   expect_errors(expect);

   lib_t foo = lib_tmp("foo");
   lib_t bar = lib_tmp("bar");

   lib_set_work(foo);
   parse_and_check(T_PACKAGE, T_CONTEXT, -1);

   lib_set_work(bar);
   parse_and_check(T_ENTITY, T_ENTITY, T_PACKAGE, T_CONTEXT);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue89)
{
   input_from_file(TESTDIR "/sem/issue89.vhd");

   tree_t e = parse_and_check(T_ENTITY, T_ARCH);
   fail_if_errors();

   tree_t p = tree_stmt(e, 0);

   tree_t c1 = tree_stmt(p, 0);
   fail_unless(tree_kind(c1) == T_PCALL);
   fail_unless(tree_loc(tree_ref(c1))->first_line == 6);

   tree_t c2 = tree_stmt(p, 1);
   fail_unless(tree_kind(c2) == T_PCALL);
   fail_unless(tree_loc(tree_ref(c2))->first_line == 10);
}
END_TEST

START_TEST(test_issue188)
{
   input_from_file(TESTDIR "/sem/issue188.vhd");

   const error_t expect[] = {
      { 29, "invalid reference to F inside pure function FILE_FUNC2" },
      {  9, "cannot declare a file object in a pure function" },
      { 46, "call procedure CALL_READ_B which references a file object" },
      { 66, "call procedure UPDATE_X which references a shared variable" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue219)
{
   input_from_file(TESTDIR "/sem/issue219.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue220)
{
   input_from_file(TESTDIR "/sem/issue220.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue224)
{
   input_from_file(TESTDIR "/sem/issue224.vhd");

   const error_t expect[] = {
      {  6, "parameter of class SIGNAL cannot have a default value" },
      { 18, "actual for formal A with class SIGNAL must not be OPEN" },
      { 24, "parameter of class VARIABLE with mode OUT or INOUT cannot have" },
      { 34, "parameter of class VARIABLE with mode OUT or INOUT cannot have" },
      { 43, "port with mode LINKAGE cannot have a default value" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH, T_ARCH, T_ARCH, T_ENTITY);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue221)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/issue221.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue236)
{
   input_from_file(TESTDIR "/sem/issue236.vhd");

   const error_t expect[] = {
      { 25,  "missing actual for port B of mode IN without a default" },
      { 37,  "missing actual for port C with unconstrained array type" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ENTITY, T_ARCH, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue239)
{
   input_from_file(TESTDIR "/sem/issue239.vhd");

   const error_t expect[] = {
      { 16,  "default value must be a static expression" },
      { 23,  "default value must be a static expression" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_interfaces)
{
   input_from_file(TESTDIR "/sem/interfaces.vhd");

   const error_t expect[] = {
      { 13,  "invalid object class for port" },
      { 17,  "invalid object class for port" },
      { 21,  "invalid object class for port" },
      { 30,  "invalid object class for generic" },
      { 34,  "invalid object class for generic" },
      { 38,  "invalid object class for generic" },
      { 41,  "procedure arguments may not have mode BUFFER" },
      { 42,  "procedure arguments may not have mode LINKAGE" },
      { 44,  "parameter of class CONSTANT must have mode IN" },
      { 45,  "parameter of class CONSTANT must have mode IN" },
      { 47,  "object C with class FILE must have file type" },
      { 48,  "object C with file type must have class FILE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_file_and_access)
{
   input_from_file(TESTDIR "/sem/file_and_access.vhd");

   const error_t expect[] = {
      { 10, "constants may not have access type" },
      { 11, "constants may not have a type with a subelement of access type" },
      { 12, "constants may not have a type with a subelement of access type" },
      { 13, "constants may not have file type" },
      { 15, "signals may not have access type" },
      { 16, "signals may not have a type with a subelement of access type" },
      { 17, "signals may not have a type with a subelement of access type" },
      { 18, "signals may not have file type" },
      { 20, "attributes may not have access type" },
      { 21, "attributes may not have a type with a subelement of access type" },
      { 22, "attributes may not have a type with a subelement of access type" },
      { 23, "attributes may not have file type" },
      { 27, "generics may not have access type" },
      { 28, "generics may not have a type with a subelement of access type" },
      { 29, "generics may not have a type with a subelement of access type" },
      { 30, "generics may not have file type" },
      { 36, "ports may not have access type" },
      { 37, "ports may not have a type with a subelement of access type" },
      { 38, "ports may not have a type with a subelement of access type" },
      { 39, "ports may not have file type" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue264)
{
   input_from_file(TESTDIR "/sem/issue264.vhd");

   const error_t expect[] = {
      { 23, "no matching operator \"&\" [INTEGER, INTEGER]" },
      { 26, "ambiguous use of operator \"&\"" },
      { 13, "candidate \"&\" [FOO, FOO return FOO_VEC1]" },
      { 14, "candidate \"&\" [FOO, FOO return FOO_VEC2]" },
      { 19, "case expression must have locally static subtype" },
      { 35, "case expression must have a discrete type or one dimensional" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue226)
{
   input_from_file(TESTDIR "/sem/issue226.vhd");

   const error_t expect[] = {
      { 20, "no visible declaration for FOO" },
      { -1, NULL }
   };
   expect_errors(expect);

   lib_t foo = lib_tmp("foo");
   lib_t bar = lib_tmp("bar");

   lib_set_work(foo);
   parse_and_check(T_PACKAGE, -1);

   lib_set_work(bar);
   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue197)
{
   input_from_file(TESTDIR "/sem/issue197.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue276)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/sem/issue276.vhd");

   lib_t foo = lib_tmp("foo");
   lib_set_work(foo);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_dwlau)
{
   input_from_file(TESTDIR "/sem/dwlau.vhd");

   const error_t expect[] = {
      { 20, "FS is not a physical unit" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_jcore1)
{
   input_from_file(TESTDIR "/sem/jcore1.vhd");

   const error_t expect[] = {
      { 15, "no visible declaration for STD_LOGIC" },
      { 33, "no visible declaration for MEM_WDATA_SEL_T" },
      { 36, "no visible declaration for SHIFTFUNC_T" },
      { 45, "no visible declaration for PIPELINE_EX_T" },
      { 47, "no visible declaration for PIPELINE_WB_T" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue293)
{
   input_from_file(TESTDIR "/sem/issue293.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue311)
{
   input_from_file(TESTDIR "/sem/issue311.vhd");

   const error_t expect[] = {
      { 32, "multiple conflicting visible declarations of EVENT_TYPE" },
      { 16, "visible declaration of EVENT_TYPE imported from WORK.P1.all" },
      { 20, "visible declaration of EVENT_TYPE imported from WORK.P2.all" },
      { 33, "type of initial value WORK.P2.EVENT_TYPE does not match" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH,
                   T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_foreign1)
{
   input_from_file(TESTDIR "/sem/foreign1.vhd");

   tree_t body = parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();

   tree_t decl = tree_decl(body, 0);
   fail_unless(icmp(tree_ident(decl), "PROC"));
   fail_unless(tree_flags(decl) & TREE_F_NEVER_WAITS);
}
END_TEST

START_TEST(test_issue316)
{
   input_from_file(TESTDIR "/sem/issue316.vhd");

   const error_t expect[] = {
      { 38, "actual for formal REG_IN with class SIGNAL must be a name" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue350)
{
   input_from_file(TESTDIR "/sem/issue350.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue246)
{
   input_from_file(TESTDIR "/sem/issue246.vhd");

   const error_t expect[] = {
      {  2, "index constraint cannot be used with non-array type INTEGER" },
      {  3, "range constraint cannot be used with non-scalar type STRING" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue356)
{
   input_from_file(TESTDIR "/sem/issue356.vhd");

   const error_t expect[] = {
      { 17, "case expression must have locally static subtype" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue359)
{
   input_from_file(TESTDIR "/sem/issue359.vhd");

   const error_t expect[] = {
      {  8, "FOO already declared in this region" },
      {  6, "previous declaration of FOO was here" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue359a)
{
   input_from_file(TESTDIR "/sem/issue359a.vhd");

   const error_t expect[] = {
      { 15, "procedure HOST_WRITE not allowed in an expression" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue368)
{
   input_from_file(TESTDIR "/sem/issue368.vhd");

   const error_t expect[] = {
      { 17, "with mode IN does not match mode OUT in specification" },
      {  6, "parameter SIG_IN was originally declared here" },
      { 18, "parameter SIG_OUT of subprogram body IN_THROUGH_THE_OUT_DOOR" },
      {  7, "parameter SIG_OUT was originally declared here" },
      { 23, "subprogram body FOO missing parameter X" },
      { 10, "parameter X was originally declared here" },
      { 27, "class constant of subprogram body BAR" },
      { 11, "parameter X was originally declared here" },
      { 31, "subprogram body BAZ missing parameter X" },
      { 12, "parameter X was originally declared here" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue363)
{
   input_from_file(TESTDIR "/sem/issue363.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_vests1)
{
   input_from_file(TESTDIR "/sem/vests1.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue369)
{
   input_from_file(TESTDIR "/sem/issue369.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue326)
{
   input_from_file(TESTDIR "/sem/issue326.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue232)
{
   input_from_file(TESTDIR "/sem/issue232.vhd");

   const error_t expect[] = {
      { 20, "sub-elements of composite port cannot be associated with OPEN" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue341)
{
   input_from_file(TESTDIR "/sem/issue341.vhd");

   const error_t expect[] = {
      { 23, "type of actual CHARACTER does not match formal B type BIT" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue340)
{
   input_from_file(TESTDIR "/sem/issue340.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue225)
{
   input_from_file(TESTDIR "/sem/issue225.vhd");

   tree_t a = parse_and_check(T_PACKAGE, T_PACKAGE, T_PACKAGE, T_PACKAGE,
                              T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   fail_if_errors();

   fail_unless(tree_contexts(a) == 8);
   fail_unless(tree_ident(tree_context(a, 3)) == ident_new("WORK.P2"));
   fail_unless(tree_ident(tree_context(a, 4)) == ident_new("WORK.P3"));
   fail_unless(tree_ident(tree_context(a, 5)) == ident_new("WORK.P4"));
   fail_unless(tree_ident(tree_context(a, 6)) == ident_new("WORK.P5"));
   fail_unless(tree_ident(tree_context(a, 7)) == ident_new("WORK.P6"));

   tree_t e = tree_primary(a);
   fail_unless(tree_contexts(e) == 4);
   fail_unless(tree_ident(tree_context(e, 3)) == ident_new("WORK.P1"));
}
END_TEST

START_TEST(test_issue377)
{
   input_from_file(TESTDIR "/sem/issue377.vhd");

   const error_t expect[] = {
      { 20, "ambiguous use of operator \"=\"" },
      {  2, "candidate \"=\" [INT_VECTOR, INT_VECTOR return BOOLEAN]" },
      {  8, "candidate \"=\" [INT_VECTOR, INT_VECTOR return BOOLEAN]" },
      { 29, "ambiguous use of operator \"=\"" },
      {  8, "candidate \"=\" [INT_VECTOR, INT_VECTOR return BOOLEAN]" },
      {  2, "candidate \"=\" [INT_VECTOR, INT_VECTOR return BOOLEAN]" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH, T_ARCH);

   set_relax_rules(RELAX_PREFER_EXPLICT);

   input_from_file(TESTDIR "/sem/issue377.vhd");

   tree_t a1 = parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH, -1);

   {
      tree_t s0 = tree_stmt(a1, 0);
      fail_unless(tree_kind(s0) == T_CONCURRENT);

      tree_t fcall = tree_value(tree_stmt(s0, 0));
      fail_unless(tree_kind(fcall) == T_FCALL);

      tree_t decl = tree_ref(fcall);
      fail_unless(tree_subkind(decl) == S_USER);
      fail_unless(tree_ident(decl) == ident_new("\"=\""));
   }

   tree_t a2 = parse_and_check(T_ARCH);

   {
      tree_t s0 = tree_stmt(a2, 0);
      fail_unless(tree_kind(s0) == T_CONCURRENT);

      tree_t fcall = tree_value(tree_stmt(s0, 0));
      fail_unless(tree_kind(fcall) == T_FCALL);

      tree_t decl = tree_ref(fcall);
      fail_unless(tree_subkind(decl) == S_USER);
      fail_unless(tree_ident(decl) == ident_new("\"=\""));
   }

   check_expected_errors();
}
END_TEST

START_TEST(test_issue386)
{
   input_from_file(TESTDIR "/sem/issue386.vhd");

   const error_t expect[] = {
      {  7, "type of parameter BYTE does not match type BIT_VECTOR" },
      {  2, "parameter BYTE was originally declared here" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_textio)
{
   input_from_file(TESTDIR "/sem/textio.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_vital1)
{
   input_from_file(TESTDIR "/sem/vital1.vhd");

   parse_and_check(T_PACKAGE);

   fail_if_errors();
}
END_TEST

START_TEST(test_physical)
{
   input_from_file(TESTDIR "/sem/physical.vhd");

   const error_t expect[] = {
      {  7, "the abstract literal portion of a secondary unit declaration" },
      { 14, "secondary unit BAR must have type MYOTHERTIME" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_block)
{
   input_from_file(TESTDIR "/sem/block.vhd");

   const error_t expect[] = {
      { 19, "missing actual for generic G1 without a default" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue407)
{
   input_from_file(TESTDIR "/sem/issue407.vhd");

   const error_t expect[] = {
      { 28, "expected type of range bound to be NATURAL but is BIT" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   tree_t s0 = tree_stmt(a, 0);
   fail_unless(tree_kind(s0) == T_CONCURRENT);

   tree_t w = tree_waveform(tree_value(tree_assoc(tree_stmt(s0, 0), 0)), 0);
   tree_t assoc = tree_assoc(tree_value(w), 0);
   fail_unless(tree_kind(assoc) == T_ASSOC);
   fail_unless(tree_subkind(assoc) == A_RANGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_resolution)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/resolution.vhd");

   const error_t expect[] = {
      {  6, "no visible subprogram declaration for BIT_VECTOR" },
      { 10, "type mark VEC does not refer to a type" },
      { 10, "unexpected identifier while parsing subtype declaration" },
      { 17, "non-record type MY_UTYPE_VECTOR may not have record element" },
      { 29, "non-composite type MY_UTYPE may not have element resolution" },
      { 33, "resolution function name PROC is not a function" },
      { 37, "parameter of resolution function must be an array of MY_UTYPE" },
      { 41, "result of resolution function must be MY_UTYPE but have UREC" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_osvvm1)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/osvvm1.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_osvvm2)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/osvvm2.vhd");

   parse_and_check(T_PACKAGE, T_PACKAGE);

   fail_if_errors();
}
END_TEST

START_TEST(test_osvvm3)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/osvvm3.vhd");

   parse_and_check(T_PACKAGE, T_PACKAGE, T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_murax)
{
   input_from_file(TESTDIR "/sem/murax.vhd");

   tree_t p1 = parse();
   fail_if(p1 == NULL);
   fail_unless(tree_kind(p1) == T_PACKAGE);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

   fail_if_errors();
}
END_TEST

START_TEST(test_driving)
{
   input_from_file(TESTDIR "/sem/driving.vhd");

   const error_t expect[] = {
      { 15, "prefix of attribute DRIVING must denote a signal" },
      { 17, "prefix of attribute DRIVING must denote a signal or a port "
        "with mode IN, INOUT, or BUFFER" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_error1)
{
   input_from_file(TESTDIR "/sem/error1.vhd");

   // There are probably too many errors generated here
   const error_t expect[] = {
      { 25, "unexpected ; while parsing port map aspect, expecting" },
      { 26, "unexpected , while parsing concurrent procedure call statement" },
      { 27, "no visible subprogram declaration for Y" },
      { 27, "unexpected , while parsing concurrent procedure call statement" },
      { 28, "no visible subprogram declaration for Z" },
      { 28, "no visible subprogram declaration for B" },
      { 23, "missing actual for port Y of mode IN without a default" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_tc251)
{
   input_from_file(TESTDIR "/sem/tc251.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_linkage)
{
   input_from_file(TESTDIR "/sem/linkage.vhd");

   const error_t expect[] = {
      {  8, "linkage port X may not be updated except" },
      {  9, "linkage port X may not be read except" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_error2)
{
   input_from_file(TESTDIR "/sem/error2.vhd");

   const error_t expect[] = {
      {  2, "unexpected to while parsing primary, expecting one of ** or )" },
      {  2, "invalid expression in range constraint" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_error3)
{
   input_from_file(TESTDIR "/sem/error3.vhd");

   const error_t expect[] = {
      {  7, "unexpected integer while parsing type definition" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_tc792)
{
   input_from_file(TESTDIR "/sem/tc792.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ENTITY, T_ENTITY,
                   T_ENTITY, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_vhdl2008)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/vhdl2008.vhd");

   const error_t expect[] = {
      { 11, "type of condition must be BOOLEAN but have INTEGER" },
      { 12, "type of value BOOLEAN does not match type of target INTEGER" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_genpack)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/genpack.vhd");

   const error_t expect[] = {
      { 45, "unit WORK.ENT is not an uninstantiated package" },
      { 46, "unit STD.STANDARD is not an uninstantiated package" },
      { 47, "missing declaration for package WORK.NOT_HERE" },
      { 48, "missing actual for generic FRAC without a default expression" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_INST,
                   T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

Suite *get_sem_tests(void)
{
   Suite *s = suite_create("sem");

   TCase *tc_core = nvc_unit_test();
   tcase_add_test(tc_core, test_integer);
   tcase_add_test(tc_core, test_ports);
   tcase_add_test(tc_core, test_scope);
   tcase_add_test(tc_core, test_ambiguous);
   tcase_add_test(tc_core, test_const);
   tcase_add_test(tc_core, test_const2);
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
   tcase_add_test(tc_core, test_static);
   tcase_add_test(tc_core, test_subtype);
   tcase_add_test(tc_core, test_universal);
   tcase_add_test(tc_core, test_issue52);
   tcase_add_test(tc_core, test_issue58);
   tcase_add_test(tc_core, test_spec);
   tcase_add_test(tc_core, test_issue53);
   tcase_add_test(tc_core, test_supersede);
   tcase_add_test(tc_core, test_implicit);
   tcase_add_test(tc_core, test_config);
   tcase_add_test(tc_core, test_protected);
   tcase_add_test(tc_core, test_protected2);
   tcase_add_test(tc_core, test_alias);
   tcase_add_test(tc_core, test_issue102);
   tcase_add_test(tc_core, test_issue105);
   tcase_add_test(tc_core, test_issue105a);
   tcase_add_test(tc_core, test_issue88);
   tcase_add_test(tc_core, test_issue128);
   tcase_add_test(tc_core, test_issue130);
   tcase_add_test(tc_core, test_issue132);
   tcase_add_test(tc_core, test_issue131);
   tcase_add_test(tc_core, test_issue133);
   tcase_add_test(tc_core, test_issue140);
   tcase_add_test(tc_core, test_issue144);
   tcase_add_test(tc_core, test_issue151);
   tcase_add_test(tc_core, test_duplicate);
   tcase_add_test(tc_core, test_issue165);
   tcase_add_test(tc_core, test_issue162);
   tcase_add_test(tc_core, test_issue178);
   tcase_add_test(tc_core, test_issue177);
   tcase_add_test(tc_core, test_use);
   tcase_add_test(tc_core, test_afunc);
   tcase_add_test(tc_core, test_issue173);
   tcase_add_test(tc_core, test_issue174);
   tcase_add_test(tc_core, test_varinit);
   tcase_add_test(tc_core, test_issue201);
   tcase_add_test(tc_core, test_issue176);
   tcase_add_test(tc_core, test_context);
   tcase_add_test(tc_core, test_issue89);
   tcase_add_test(tc_core, test_issue188);
   tcase_add_test(tc_core, test_issue219);
   tcase_add_test(tc_core, test_issue220);
   tcase_add_test(tc_core, test_issue224);
   tcase_add_test(tc_core, test_issue221);
   tcase_add_test(tc_core, test_issue236);
   tcase_add_test(tc_core, test_issue239);
   tcase_add_test(tc_core, test_interfaces);
   tcase_add_test(tc_core, test_file_and_access);
   tcase_add_test(tc_core, test_issue264);
   tcase_add_test(tc_core, test_issue226);
   tcase_add_test(tc_core, test_issue197);
   tcase_add_test(tc_core, test_issue276);
   tcase_add_test(tc_core, test_dwlau);
   tcase_add_test(tc_core, test_jcore1);
   tcase_add_test(tc_core, test_issue293);
   tcase_add_test(tc_core, test_issue311);
   tcase_add_test(tc_core, test_foreign1);
   tcase_add_test(tc_core, test_issue316);
   tcase_add_test(tc_core, test_issue350);
   tcase_add_test(tc_core, test_issue246);
   tcase_add_test(tc_core, test_issue356);
   tcase_add_test(tc_core, test_issue359);
   tcase_add_test(tc_core, test_issue359a);
   tcase_add_test(tc_core, test_issue368);
   tcase_add_test(tc_core, test_issue363);
   tcase_add_test(tc_core, test_vests1);
   tcase_add_test(tc_core, test_issue369);
   tcase_add_test(tc_core, test_issue326);
   tcase_add_test(tc_core, test_issue232);
   tcase_add_test(tc_core, test_issue341);
   tcase_add_test(tc_core, test_issue340);
   tcase_add_test(tc_core, test_issue225);
   tcase_add_test(tc_core, test_issue377);
   tcase_add_test(tc_core, test_issue386);
   tcase_add_test(tc_core, test_textio);
   tcase_add_test(tc_core, test_vital1);
   tcase_add_test(tc_core, test_physical);
   tcase_add_test(tc_core, test_block);
   tcase_add_test(tc_core, test_issue407);
   tcase_add_test(tc_core, test_resolution);
   tcase_add_test(tc_core, test_osvvm1);
   tcase_add_test(tc_core, test_osvvm2);
   tcase_add_test(tc_core, test_osvvm3);
   tcase_add_test(tc_core, test_murax);
   tcase_add_test(tc_core, test_driving);
   tcase_add_test(tc_core, test_error1);
   tcase_add_test(tc_core, test_tc251);
   tcase_add_test(tc_core, test_linkage);
   tcase_add_test(tc_core, test_error2);
   tcase_add_test(tc_core, test_error3);
   tcase_add_test(tc_core, test_tc792);
   tcase_add_test(tc_core, test_vhdl2008);
   tcase_add_test(tc_core, test_genpack);
   suite_add_tcase(s, tc_core);

   return s;
}
