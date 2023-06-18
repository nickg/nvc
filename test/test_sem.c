//
//  Copyright (C) 2011-2022  Nick Gasson
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

#include "test_util.h"
#include "common.h"
#include "diag.h"
#include "lib.h"
#include "option.h"
#include "phase.h"
#include "scan.h"
#include "type.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   tree_t a, d, p, s, e;
   type_t t;

   opt_set_int(OPT_WARN_HIDDEN, 1);

   const error_t expect[] = {
      { 20, "MY_INT1 does not match type of target MY_INT2" },
      { 30, "MY_INT1 does not match type of target MY_INT2_SUB" },
      { 35, "no visible declaration for NOTHING" },
      { 48, "no matching operator \"+\" [ANOTHER_ONE, universal_integer " },
      { 48, "no matching operator \"+\" [MY_INT1, universal_integer return "},
      { 52, "declaration of variable B hides entity B" },
      { 57, "MY_INT2 has no attribute CAKE" },
      { 61, "right bound must be of some integer type but have universal" },
      { 62, "declaration of type X hides signal X" },
      { 63, "range bounds must be of some integer type but have BOOLEAN" },
      { -1, NULL }
   };
   expect_errors(expect);

   input_from_file(TESTDIR "/sem/integer.vhd");

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

   fail_unless(parse() == NULL);

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
   opt_set_int(OPT_MISSING_BODY, 1);
   opt_set_int(OPT_WARN_HIDDEN, 1);

   input_from_file(TESTDIR "/sem/ports.vhd");

   const error_t expect[] = {
      { 31,  "cannot read output port O" },
      { 42,  "cannot assign to input port I" },
      { 81,  "missing actual for port I of mode IN without a default " },
      { 85,  "formal port I already has an actual" },
      { 89,  "at least 3 positional actuals but WORK.FOO has only 2 ports" },
      { 92,  "WORK.FOO has no port named CAKE" },
      { 94,  "design unit BAD not found in library WORK" },
      { 103, "unconnected port I with mode IN must have a default value" },
      { 116, "object X is not a component declaration" },
      { 122, "declaration of signal X hides signal X" },
      { 123, "declaration of signal Y hides signal Y" },
      { 148, "port O of mode OUT must be a static signal name or OPEN" },
      { 155, "BAR has no port named Q" },
      { 163, "BAR has no port named U" },
      { 168, "formal name must be locally static" },
      { 177, "formal name must be locally static" },
      { 185, "no visible subprogram declaration for HELLO" },
      { 217, "port O of unconstrained type INT_VEC cannot be unconnected" },
      { 221, "type of actual universal_real does not match type INTEGER" },
      { 272, "result of conversion for unconstrained formal I must" },
      { 280, "port N of mode IN must be a globally static expression or " },
      { 284, "conversion not allowed for formal O with mode OUT" },
      { 294, "output conversion not allowed for formal I with mode IN" },
      { 298, "output conversion for formal B must not have OPEN actual" },
      { 304, "port B of mode INOUT must be a static signal name or OPEN" },
      { 307, "port B of mode INOUT must be a static signal name or OPEN" },
      { 256, "missing body for function FUNC1 [BIT return MY_INT1]" },
      { 257, "missing body for function FUNC2 [BIT, INTEGER return MY_INT1]" },
      { 258, "missing body for function FUNC3 [BIT return INTEGER]" },
      { 259, "missing body for function FUNC4 [INTEGER return BIT]" },
      { 318, "cannot assign to input port X" },
      { 319, "cannot read output port Y" },
      { 339, "parameter X of mode IN with formal X of mode OUT" },
      { 363, "associated with port I of mode IN must be a globally static" },
      { 367, "actual associated with port I of mode IN must be a globally " },
      { 375, "invalid output conversion \"not\"" },
      { 373, "missing actual for port I of mode IN without a default " },
      { 353, "missing body for function \"not\" [INTEGER return INTEGER]" },
      { 354, "missing body for function FOO" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/scope.vhd");

   const error_t expect[] = {
      {  18, "declaration of signal A hides architecture A" },
      {  31, "WORK.PACK1.MY_INT1 does not match type"
         " of target WORK.PACK2.MY_INT1" },
      {  44, "WORK.PACK1.MY_INT1 does not match type of target "
         "WORK.NO_USE_CLAUSE-A.MY_INT1" },
      {  63, "G already declared in this region" },
      {  71, "P already declared in this region" },
      {  82, "declaration of variable P hides signal P" },
      {  83, "declaration of variable G hides constant G" },
      { 114, "no visible declaration for MY_INT1" },
      { 137, "no visible declaration for E1" },
      { 160, "no visible subprogram declaration for FUNC2" },
      { 167, "object NOT_HERE not found in unit WORK.PACK5" },
      { 189, "no visible declaration for MY_INT1" },
      { 236, "no visible declaration for FOO" },
      { 302, "declaration of constant I hides constant I" },
      { 306, "name X not found in L1" },
      { 321, "declaration of constant X hides variable X" },
      { 330, "declaration of variable X hides constant X" },
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

   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/ambiguous.vhd");

   const error_t expect[] = {
      {  35, "type of value BAR does not match type of target FOO" },
      {  49, "declaration of variable X hides signal X" },
      {  56, "type of aggregate cannot be determined" },
      {  56, "type of aggregate cannot be determined" },
      {  75, "declaration of variable X hides signal X" },
      {  76, "declaration of variable Y hides signal Y" },
      {  86, "ambiguous use of enumeration literal FALSE" },
      {  93, "ambiguous use of name NOW" },
      { 103, "ambiguous use of name FALSE" },
      { 113, "declaration of variable X hides signal X" },
      { 113, "declaration of variable Y hides signal Y" },
      { 141, "ambiguous use of operator \"<\"" },
      { 222, "type of aggregate cannot be determined" },
      { 222, "type of aggregate cannot be determined" },
      { -1, NULL }
   };
   expect_errors(expect);

   e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

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
   lib_put(lib_work(), p);

   fail_unless(error_count() == 0);

   fail_unless(tree_kind(p) == T_PACKAGE);

   tree_t d = search_decls(p, ident_new("DEF_ARR"), 0);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(type_is_unconstrained(tree_type(d)));
   fail_if(tree_has_value(d));

   tree_t pb = parse();
   fail_if(pb == NULL);
   fail_unless(tree_kind(pb) == T_PACK_BODY);

   fail_if_errors();
}
END_TEST

START_TEST(test_std)
{
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/std.vhd");

   const error_t expect[] = {
      { 22, "declaration of variable A hides entity A" },
      { 23, "declaration of variable B hides architecture B" },
      { 28, "declaration of variable T hides signal T" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_wait)
{
   input_from_file(TESTDIR "/sem/wait.vhd");

   const error_t expect[] = {
      { 17, "type of delay must be TIME" },
      { 26, "name V in sensitivity list is not a signal" },
      { 35, "invalid use of entity A" },
      { 40, "wait statement not allowed in process" },
      { 51, "type of condition must be BOOLEAN" },
      { 53, "type of delay must be TIME" },
      { 66, "name in sensitivity list is not a static signal name" },
      { 69, "name in sensitivity list is not a static signal name" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

   tree_t a = parse();
   fail_if(a == NULL);
   fail_unless(tree_kind(a) == T_ARCH);

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
      {   5, "function arguments must have mode IN" },
      {  19, "must be an unconstrained array type" },
      {  23, "resolution function must have a single argument" },
      {  25, "no visible subprogram declaration for UENUM" },
      {  27, "type of default value universal_integer does not" },
      {  29, "subprogram body is not allowed in package specification" },
      {  36, "design unit BAD not found in library WORK" },
      {  48, "expected return type INTEGER but have UENUM" },
      {  51, "function arguments must have mode IN" },
      {  62, "FOO [INTEGER, INTEGER, INTEGER return INTEGER] already " },
      { 114, "positional parameters must precede named parameters" },
      { 115, "formal parameter X already has an associated actual" },
      { 124, "function arguments may not have VARIABLE class" },
      { 146, "B with class VARIABLE must be a name denoting a variable" },
      { 161, "pure function TEST18 cannot call impure function" },
      { 166, "formal parameter X with access type must have class VARIABLE" },
      { 180, "universal_real does not match formal X type INTEGER" },
      { 181, "missing actual for formal parameter Y without default value" },
      { 182, "type of actual universal_integer does not match formal Y" },
      { 239, "class variable of subprogram body TEST25 parameter" },
      { 245, "class constant of subprogram body TEST26 parameter" },
      { 271, "invalid reference to X inside pure function NESTED" },
      { 288, "no visible subprogram declaration for FNORK" },
      { 293, "function CONSTPURE cannot be called as a procedure" },
      { 294, "procedure NOTDEF not allowed in an expression" },
      { 297, "no visible declaration for BAD_TYPE" },
      { 297, "no visible declaration for FOO" },
      { 303, "default value of parameter X in subprogram body FUNC1" },
      { 310, "default value of parameter X in subprogram body FUNC2" },
      { 317, "default value of parameter X in subprogram body FUNC3" },
      { 324, "function FUNC4 declaration was pure but body is impure" },
      { 331, "function FUNC5 declaration was impure but body is pure" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/array.vhd");

   const error_t expect[] = {
      { 27,  "positional associations must appear first in aggregate" },
      { 33,  "named association must not follow others" },
      { 39,  "only a single others association allowed" },
      { 45,  "declaration of variable A hides signal A" },
      { 46,  "type of initial value universal_integer does not match" },
      { 51,  "declaration of variable A hides signal A" },
      { 55,  "type of value universal_integer does not match type of" },
      { 57,  "type of value INT_ARRAY does not match type" },
      { 65,  "operator \"=\" [INT_ARRAY, TEN_INTS return BOOLEAN]" },
      { 88,  "array W has 2 dimensions but 1 indices given" },
      { 89,  "array W has 2 dimensions but 3 indices given" },
      { 98,  "type of index universal_integer does not match type" },
      { 102, "named and positional associations cannot be mixed in" },
      { 107, "declaration of variable X hides signal X" },
      { 109, "declaration of variable Y hides signal Y" },
      { 111, "a choice that is not locally static is allowed" },
      { 116, "declaration of variable X hides signal X" },
      { 117, "declaration of variable Y hides signal Y" },
      { 119, "type of aggregate cannot be determined from the surrounding" },
      { 119, "type of slice prefix INTEGER is not an array" },
      { 120, "range direction of slice TO does not match prefix DOWNTO" },
      { 121, "index range of array aggregate with others choice cannot" },
      { 126, "declaration of variable X hides signal X" },
      { 130, "ambiguous use of enumeration literal '0'" },
      { 130, "range direction of slice DOWNTO does not match prefix TO" },
      { 146, "declaration of constant A hides signal A" },
      { 207, "array BAD cannot have unconstrained element type" },
      { 215, "array aggregate with others choice cannot be determined" },
      { 232, "aliased name is not static" },
      { 233, "aliased name is not static" },
      { 234, "type of aliased object INT_ARRAY does not match" },
      { 241, "cannot index non-array type INTEGER" },
      { 252, "expected 1 constraints for type INT_ARRAY but found 2" },
      { 269, "no matching operator \"<\" [TEN_TEN_INTS, TEN_TEN_INTS " },
      { 270, "no matching operator \">\" [TEN_TEN_INTS, TEN_TEN_INTS " },
      { 272, "no matching operator \"<\" [INT2D, INT2D" },
      { 277, "no visible declaration for NOT_HERE" },
      { 279, "variable A2 cannot have unconstrained type NUM_ARRAY" },
      { 285, "name K in discrete range does not refer to a type" },
      { 285, "declaration of type A hides signal A" },
      { 292, "declaration of variable A hides signal A" },
      { 295, "type of index universal_integer does not match" },
      { 308, "declaration of constant X hides signal X" },
      { 335, "declaration of variable X hides signal X" },
      { 343, "invalid character 'f' in string literal of type BIT_VECTOR" },
      { 352, "declaration of constant A hides signal A" },
      { 365, "cannot change constraints of constrained array type TEN_INTS" },
      { 366, "cannot change constraints of constrained array type TEN_INTS" },
      { 379, "array T_FILE_ARRAY cannot have element of file type" },
      { 391, "index type REAL is not discrete" },
      { 392, "index type BIT_VECTOR is not discrete" },
      { 399, "type of array aggregate choice BOOLEAN does not match INT_" },
      { 400, "in range: left is universal_integer, right is BOOLEAN" },
      { 401, "expected type of range bounds to be INTEGER but have BOOLEAN" },
      { 403, "a choice that is not locally static is allowed" },
      { 404, "a choice that is not locally static is allowed" },
      { 424, "ambiguous call to function F" },
      { 436, "type mismatch in range: left is universal_real, right is" },
      { 446, "no visible declaration for FOO" },
      { 464, "declaration of variable A hides signal A" },
      { 480, "cannot index non-array type MY_RECORD" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   opt_set_int(OPT_MISSING_BODY, 1);
   input_from_file(TESTDIR "/sem/generics.vhd");

   const error_t expect[] = {
      {  34, "missing actual for generic N" },
      {  38, "2 positional actuals but WORK.BOT has only 1 generic" },
      {  48, "no visible declaration for X" },
      {  58, "invalid object class signal for generic Y" },
      {  68, "no visible declaration for Y" },
      {  67, "declaration of signal P hides package P" },
      {  91, "declaration of constant X hides constant X" },
      { 110, "with generic X must be a globally static expression" },
      { 116, "unexpected integer while parsing name" },
      { 115, "missing actual for generic X without a default expression" },
      { 119, "invalid name in generic map" },
      { 118, "missing actual for generic X without a default expression" },
      { 122, "invalid use of label I6" },
      { 125, "invalid use of architecture A" },
      { 128, "invalid use of package STD.STANDARD" },
      { 131, "formal generic name must be a locally static name" },
      {  87, "missing body for function F [BIT_VECTOR return INTEGER]" },
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
      {  15, "type of condition must be BOOLEAN" },
      {  19, "no visible declaration for X" },
      {  25, "type of value BOOLEAN does not match type of target INTEGER" },
      {  33, "no visible declaration for G" },
      {  48, "return statement not allowed outside subprogram" },
      {  62, "return statement not allowed outside subprogram" },
      {  64, "type of loop condition must be BOOLEAN" },
      {  79, "no visible declaration for X" },
      { 107, "others choice must appear last" },
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
      { 190, "cannot use next statement outside loop" },
      { 192, "no visible declaration for FOO" },
      { 205, "DUP already declared in this region" },
      { 214, "type REAL does not have a range" },
      { 222, "variable I is not a valid target of signal assignment" },
      { 228, "expected type mark while parsing discrete range" },
      { 230, "range bounds to be INTEGER but have universal_real" },
      { 232, "type of range bounds REAL is not discrete" },
      { 243, "target of variable assignment must be a variable name" },
      { 244, "aggregate element must be locally static name" },
      { 245, "others association not allowed in aggregate variable target" },
      { 246, "range association not allowed in aggregate variable target" },
      { 254, "X with class SIGNAL must be a name denoting a signal" },
      { 255, "X with class SIGNAL must be a name denoting a signal" },
      { 263, "target of variable assignment must be a variable name or" },
      { 264, "actual for formal X with class VARIABLE must be a name" },
      { 270, "no visible declaration for BAD_TYPE" },
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
      {  28, "cannot return a value from a procedure" },
      {  45, "type of default value universal_integer does not match" },
      {  63, "missing actual for formal parameter X without default value" },
      {  64, "positional parameters must precede named parameters" },
      {  83, "cannot read OUT parameter Y" },
      {  84, "target of variable assignment must be a variable name" },
      {  90, "implicit signal STABLE cannot be used in a subprogram body" },
      {  91, "implicit signal QUIET cannot be used in a subprogram body" },
      {  92, "implicit signal TRANSACTION cannot be used in a subprogram" },
      {  93, "implicit signal DELAYED cannot be used in a subprogram" },
      {  98, "formal parameter X with access type must have class VARIABLE" },
      {  99, "formal parameter X with access type must have class VARIABLE" },
      { 100, "formal parameter X with access type must have class VARIABLE" },
      { 137, "ambiguous use of enumeration literal '0'" },
      { 137, "ambiguous use of enumeration literal '1'" },
      { 142, "cannot read OUT parameter X" },
      { 148, "X is not a valid target of signal assignment" },
      { 157, "formal parameter ARG with type containing an access type " },
      { 162, "formal parameter ARG with type containing an access type " },
      { 167, "formal parameter ARG with type containing an access type " },
      { 172, "formal parameter ARG with type containing an access type " },
      { 180, "expected procedure name" },
      { 183, "declaration may not include the reserved word BUS" },
      { 193, "signal parameter Y must be denoted by a static signal name" },
      { 201, "formal parameter X already has an associated actual" },
      { 202, "no possible overload of TEST17_A has formal Z" },
      { 207, "cannot assign to input parameter X" },
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
      {  65, "expected attribute type INTEGER" },
      {  66, "expected attribute type STRING" },
      {  67, "no visible declaration for Q" },
      {  85, "dimension of attribute LEFT must be locally static" },
      { 101, "prefix of SIMPLE_NAME attribute must be a named entity" },
      { 103, "prefix of PATH_NAME attribute must be a named entity" },
      { 133, "cannot index non-array type INTEGER" },
      { 139, "class of object I is variable not signal" },
      { 146, "prefix of attribute LAST_EVENT must denote a signal" },
      { 158, "attribute RANGE with unconstrained array type BIT_VECTOR" },
      { 159, "object B does not have a range" },
      { 160, "prefix does not have a range" },
      { 204, "prefix does not have LENGTH attribute" },
      { 212, "cannot use attribute IMAGE with non-scalar type INT2_VEC" },
      { 221, "prefix of 'BASE attribute must be a type or subtype declara" },
      { 222, "BASE attribute is allowed only as the prefix of the name" },
      { 228, "no visible declaration for NOT_HERE" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/generate.vhd");

   const error_t expect[] = {
      { 15, "type of condition must be BOOLEAN but have INTEGER" },
      { 26, "no visible declaration for Y" },
      { 33, "declaration of constant X hides signal X" },
      { 39, "declaration of constant X hides signal X" },
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
      {  30, "record field X cannot have unconstrained array type" },
      {  39, "field Z does not have a value" },
      {  40, "does not match type INTEGER of field Y" },
      {  42, "field Y does not have a value" },
      {  43, "record type R1 has no field named Q" },
      {  44, "type of value R1 does not match type INTEGER of" },
      {  47, "field X was already given a value by earlier named choice" },
      {  48, "field X was already given a value by earlier positional choice" },
      {  64, "variable A1 cannot have unconstrained type R1_VEC" },
      {  72, "record type R1 has no field named F" },
      {  82, "record type R1_SUB has no field named Z" },
      {  86, "index constraint cannot be used with non-array type R1" },
      { 106, "record type R1 has no field named Z" },
      { 111, "record field A cannot be of file type" },
      { 124, "record type R8 has no field named ACK" },
      { 153, "cannot index non-array type UNIT_SPEC_T" },
      { 155, "cannot index non-array type UNIT_SPEC_T" },
      { 157, "cannot index non-array type UNIT_SPEC_T" },
      { 166, "association choice must be a field name" },
      { 167, "3 positional associations given but record type R1 only has 2" },
      { 168, "others association must represent at least one element" },
      { 169, "range association invalid in record aggregate" },
      { 170, "record type R1 has no field named Z" },
      { 170, "range association invalid in record aggregate" },
      { 174, "no visible declaration for FOO" },
      { 184, "type INTEGER is not a record" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();

   tree_t d = search_decls(p, ident_new("P9"), 0);
   fail_if(d == NULL);
   d = tree_decl(d, 0);
   fail_unless(tree_kind(d) == T_VAR_DECL);
   fail_if(tree_has_value(d));
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/access.vhd");

   const error_t expect[] = {
      {   5, "no visible declaration for FOO" },
      {  34, "null expression must have access type" },
      {  38, "unexpected integer while parsing name, expecting" },
      {  39, "type mark I does not denote a type or a subtype" },
      {  41, "does not match type of target INT_PTR" },
      {  47, "type of value REC does not match type of" },
      {  55, "type of allocator expresion INTEGER does not match" },
      {  56, "type mark S does not denote a type or a subtype" },
      {  76, "unconstrained array type INT_PTR_ARRAY not allowed" },
      {  84, "index constraint cannot be used with non-array type INTEGER" },
      {  90, "variable F cannot have incomplete type FOO" },
      {  97, "cannot determine type of allocator expression from the surro" },
      { 103, "declaration of variable P hides package P" },
      { 105, "incomplete type A found in allocator expression" },
      { 109, "declaration of variable P hides package P" },
      { 111, "expression type INT_VEC is not access" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_real)
{
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/real.vhd");

   const error_t expect[] = {
      { 16, "type of value MY_REAL does not match type of target" },
      { 25, "conversion only allowed between closely related types" },
      { 29, "declaration of variable X hides signal X" },
      { 38, "type of right bound must be of some real type but have INTEGER" },
      { 39, "type of right bound must be of some integer type but have REAL" },
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
      { 23, "design unit E-INVALID not found in library WORK" },
      { 26, "design unit WORK.PACK is not an entity" },
      { 30, "unit WORK.PACK is not an entity" },
      { 61, "signal assignment statement not allowed inside passive process" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_PACKAGE, T_ARCH, T_ARCH, T_ENTITY,
                   T_ARCH, T_ENTITY);

   check_expected_errors();
}
END_TEST

START_TEST(test_signal)
{
   input_from_file(TESTDIR "/sem/signal.vhd");

   const error_t expect[] = {
      { 14, "type of aggregate cannot be determined from the surrounding" },
      { 15, "type of string literal cannot be determined from the surrou" },
      { 16, "target of signal assignment must be a signal name" },
      { 17, "others association not allowed in aggregate signal target" },
      { 18, "cannot assign to input port P" },
      { 22, "cannot be determined from the surrounding context" },
      { 23, "target of signal assignment must be a signal name" },
      { 24, "others association not allowed in aggregate signal target" },
      { 25, "cannot assign to input port P" },
      { 30, "aggregate element must be locally static name" },
      { 40, "signal X is not a formal parameter and subprogram PROC1 [BIT] "
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
      { 88, "invalid use of entity E" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_static)
{
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/static.vhd");

   const error_t expect[] = {
      {  42, "case choice must be locally static" },
      {  65, "with port X of mode IN must be a globally static" },
      {  74, "declaration of signal X hides signal X" },
      {  85, "formal name must be locally static" },
      { 104, "no visible subprogram declaration for BAD_FUNC" },
      { 104, "declaration of constant N hides constant N" },
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
      {  9, "expected type of range bound to be INTEGER but is" },
      { 16, "no visible subprogram declaration for NOT_HERE" },
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
      { 24, "duplicate specification for instance I1" },
      { 32, "duplicate specification for instance I1" },
      { 34, "design unit NOT_HERE not found in library WORK" },
      { 36, "design unit WORK.P is not an entity" },
      { 36, "duplicate specification for instance I3" },
      { 22, "component mismatch for instance I1: expected C1" },
      { 30, "specification may only be used with component instances" },
      { 28, "instance BAD not found" },
      { 40, "instance I5 not found" },
      { 79, "component mismatch for instance I1: expected C1 but " },
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
      { 18, "WORK.PORTLISTTEST has no port named A" },
      { 19, "WORK.PORTLISTTEST has no port named B" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   opt_set_int(OPT_MISSING_BODY, 1);
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/protected.vhd");

   const error_t expect[] = {
      {  13, "no visible declaration for NOT_HERE" },
      {  19, "no visible declaration for BAD2" },
      {  22, "object BOOLEAN is not a protected type declaration" },
      {  25, "object NOW is not a protected type declaration" },
      {  47, "SHAREDCOUNTER already declared in this region" },
      {  50, "subtypes may not have protected base types" },
      {  52, "shared variable X must have protected type" },
      {  56, "variable Z with protected type may not have an initial value" },
      {  58, "function result subtype may not denote a protected type" },
      {  64, "parameter with protected type cannot have a default value" },
      { 118, "invalid use of name COUNTER" },
      { 119, "too many positional parameters for subprogram DECREMENT [INTEG" },
      { 124, "formal parameter X with protected type must have class VARIABLE" },
      { 124, "declaration of constant X hides variable X" },
      { 126, "pure function GET_VALUE cannot call impure function VALUE" },
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
      { 41, "port P1 with class signal cannot be declared with protected "
        "type T_PROTECTED" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_alias)
{
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/alias.vhd");

   const error_t expect[] = {
      { 10, "non-object alias may not have subtype indication" },
      { 12, "type mark AX does not denote a type or a subtype" },
      { 22, "no visible subprogram FOO matches signature [INTEGER "
        "return INTEGER]" },
      { 23, "no visible subprogram FOO matches signature [BIT]" },
      { 24, "name cannot be indexed or sliced" },
      { 25, "no visible declaration for BLAH" },
      { 32, "no visible subprogram BAR matches signature [INTEGER]" },
      { 40, "ambiguous use of enumeration literal '1'" },
      { 41, "no visible subprogram declaration for FOO_INT" },
      { 42, "type of actual CHARACTER does not match formal X type BIT" },
      { 43, "operand of qualified expression must have type CHARACTER" },
      { 48, "declaration of variable X hides signal X" },
      { 50, "aliased name is not static" },
      { 68, "unexpected identifier while parsing subtype declaration" },
      { 81, "object alias may not have multidimensional array type" },
      { 83, "invalid use of alias INT_VECTOR" },
      { 88, "aliased name is not static" },
      { 89, "aliased name is not static" },
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
      { 20, "duplicate subprogram body PROC [INTEGER]" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/afunc.vhd");

   const error_t expect[] = {
      { 34, "declaration of constant A hides type A" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   tree_t f = tree_value(tree_stmt(tree_stmt(a, 0), 0));
   fail_unless(tree_kind(f) == T_FCALL);

   tree_t r = tree_value(tree_param(f, 0));
   fail_unless(tree_kind(r) == T_ARRAY_REF);

   tree_t c = tree_value(r);
   fail_unless(tree_kind(c) == T_FCALL);
   fail_unless(icmp(tree_ident(c), "GET"));
   fail_unless(tree_params(c) == 0);

   check_expected_errors();
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
   parse_and_check(T_ENTITY, T_ENTITY, T_PACKAGE, T_CONTEXT, T_CONTEXT,
                   T_ENTITY);

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
      {  9, "cannot declare a file object in a pure function" },
      { 29, "invalid reference to F inside pure function FILE_FUNC2" },
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
      { 18, "formal A with class SIGNAL must be a name denoting a signal" },
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
      { 13,  "ports may not have variable class in VHDL-1993" },
      { 17,  "invalid object class constant for port P" },
      { 21,  "invalid object class file for port P" },
      { 30,  "invalid object class signal for generic P" },
      { 34,  "invalid object class variable for generic P" },
      { 38,  "invalid object class file for generic P" },
      { 41,  "subprogram formal parameters cannot have mode BUFFER" },
      { 42,  "subprogram formal parameters cannot have mode LINKAGE" },
      { 44,  "parameter of class CONSTANT must have mode IN" },
      { 45,  "parameter of class CONSTANT must have mode IN" },
      { 47,  "formal parameter C with class FILE must have file type" },
      { 48,  "formal parameter C with file type must have class FILE" },
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
      { 36, "port P1 cannot be declared with access type T_INT_ACCESS" },
      { 37, "port P2 cannot be declared with type T_ACCESS_ARRAY which has "
        "a subelement of access type" },
      { 38, "port P3 cannot be declared with type T_ACCESS_RECORD which has "
        "a subelement of access type" },
      { 39, "port P4 cannot be declared with file type T_INT_FILE" },
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
      { 19, "case expression must have locally static subtype" },
      { 23, "no matching operator \"&\" [INTEGER, INTEGER]" },
      { 26, "ambiguous use of operator \"&\"" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/issue359.vhd");

   const error_t expect[] = {
      {  8, "FOO already declared in this region" },
      {  8, "declaration of signal FOO hides signal FOO" },
      { 16, "cannot index non-array type INTEGER" },
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
      { 15, "target of signal assignment must be a signal name or aggregate" },
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
      { 18, "parameter SIG_OUT of subprogram body IN_THROUGH_THE_OUT_DOOR" },
      { 23, "parameter name Y in subprogram FOO body does not match name "
        "X in declaration" },
      { 27, "class constant of subprogram body BAR" },
      { 31, "parameter name Y in subprogram BAZ body does not match name X" },
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
   opt_set_int(OPT_WARN_HIDDEN, 1);
   input_from_file(TESTDIR "/sem/vests1.vhd");

   lib_t foo = lib_tmp("foo");
   lib_set_work(foo);

   const error_t expect[] = {
      { 32, "declaration of type C10S03B00X00P07N01I00914PKG hides package "
        "C10S03B00X00P07N01I00914PKG" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
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

   fail_unless(tree_contexts(a) == 3);

   tree_t e = tree_primary(a);
   fail_unless(tree_contexts(e) == 3);
}
END_TEST

START_TEST(test_issue377)
{
   input_from_file(TESTDIR "/sem/issue377.vhd");

   const error_t expect[] = {
      { 20, "ambiguous use of operator \"=\"" },
      { 29, "ambiguous use of operator \"=\"" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH, T_ARCH);

   opt_set_int(OPT_RELAXED, 1);

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

   tree_t sel = tree_stmt(s0, 0);
   fail_unless(tree_kind(sel) == T_SELECT);

   tree_t alt0 = tree_stmt(sel, 0);
   fail_unless(tree_kind(alt0) == T_ALTERNATIVE);

   tree_t w = tree_waveform(tree_stmt(alt0, 0), 0);
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
      { 10, "type mark VEC does not denote a type or a subtype" },
      { 10, "expecting a discrete range" },
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
   lib_put(lib_work(), p1);

   tree_t p2 = parse();
   fail_if(p2 == NULL);
   fail_unless(tree_kind(p2) == T_PACKAGE);
   lib_put(lib_work(), p2);

   tree_t e = parse();
   fail_if(e == NULL);
   fail_unless(tree_kind(e) == T_ENTITY);
   lib_put(lib_work(), e);

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

   const error_t expect[] = {
      { 25, "unexpected ; while parsing port map aspect, expecting" },
      { 23, "missing actual for port Y of mode IN without a default" },
      { 26, "expected concurrent statement" },
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
      {  5, "no visible subprogram declaration for IFELSE" },
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
      { 13, "type of condition must be BOOLEAN but have INTEGER" },
      { 14, "expected type of conditional expression to be INTEGER "
        "but is BOOLEAN" },
      { 31, "case choice must be locally static" },
      { 40, "no visible declaration for BAZ" },
      { 41, "SUBTYPE attribute is only allowed in a type mark" },
      { 49, "case choice must be globally static" },
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
      { 47, "design unit NOT_HERE not found in library WORK" },
      { 48, "missing actual for generic FRAC without a default expression" },
      { 77, "unit STD.STANDARD is not an uninstantiated package" },
      { 82, "actual for generic FIXED_PKG is not an instantiated package " },
      { 91, "expected an instance of package WORK.MYFIXED but have instance "
        "of WORK.MYFLOAT for generic FIXED_PKG" },
      { 94, "actual for generic FIXED_PKG is not an instantiated package" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_INST,
                   T_ENTITY, T_ARCH, T_PACKAGE, T_PACK_BODY, T_PACKAGE,
                   T_PACK_INST, T_ARCH, T_PACKAGE, T_PACK_INST, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_gentype)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/gentype.vhd");

   const error_t expect[] = {
      { 10, "range constraint cannot be used with non-scalar type T" },
      { 66, "type of actual universal_real does not match type T of" },
      { 72, "unexpected integer while parsing name" },
      { 72, "type of actual universal_integer does not match type T of " },
      { 81, "no visible subprogram MY_FUNC matches signature [T return T]" },
      { 80, "missing actual for generic T without a default expression" },
      { 80, "no visible subprogram PROC1 matches signature [T]" },
      { 84, "no visible subprogram MY_FUNC matches signature [T return T]" },
      { 83, "no visible subprogram PROC1 matches signature [T]" },
      { 90, "ambiguous use of name MY_FUNC" },
      { 90, "universal_integer does not match type T of formal generic INIT" },
      { 93, "invalid use of label U12" },
      { 93, "universal_integer does not match type T of formal generic INIT" },
      { 96, "universal_integer does not match type T of formal generic INIT" },
      { 95, "cannot find predefined \"=\" operator for type INCOMPLETE" },
      { 95, "cannot find predefined \"/=\" operator for type INCOMPLETE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_record2008)
{
   opt_set_int(OPT_WARN_HIDDEN, 1);
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/record2008.vhd");

   const error_t expect[] = {
      { 10,  "declaration of signal S1 cannot have unconstrained type REC1" },
      { 12,  "record type REC1 has no field named Y" },
      { 13,  "record type REC1 has no field named R2" },
      { 22,  "duplicate record element constraint for field X" },
      { 23,  "record type REC2 has no field named P" },
      { 24,  "field B in record element constraint is already constrained" },
      { 25,  "declaration of signal R8 cannot have unconstrained type REC2" },
      { 30,  "declaration of signal R10 cannot have unconstrained type T2" },
      { 34,  "cannot change constraints of constrained array type BIT_VECTOR" },
      { 41,  "declaration of signal R12 cannot have unconstrained type REC3" },
      { 43,  "declaration of signal R14 cannot have unconstrained type REC3" },
      { 68,  "declaration of constant C1 hides constant C1" },
      { 110, "variable A1 cannot have unconstrained type REC1_ARRAY" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_osvvm4)
{
   input_from_file(TESTDIR "/sem/osvvm4.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_agg2008)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/agg2008.vhd");

   const error_t expect[] = {
      { 16, "type of positional association universal_real does not match "
        "aggregate element type INTEGER or the aggregate type itself "
        "INTEGER_VECTOR" },
      { 17, "type of named association BOOLEAN does not match aggregate "
        "element type INTEGER" },
      { 19, "type of named association INTEGER_VECTOR does not match "
        "aggregate element type INTEGER" },
      { 30, "second dimension of 2 dimensional array type INT2D must be "
        "specified by a sub-aggregate, string, or bit-string literal" },
      { 71, "variable V1 cannot have unconstrained type T_BYTE_ARRAY" },
      { 73, "array constraint cannot be used with non-array type INTEGER" },
      { 74, "cannot change constraints of constrained array type BIT_VECTOR" },
      { 75, "cannot change constraints of constrained array type BIT_VECTOR" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_force)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/force.vhd");

   const error_t expect[] = {
      { 13, "variable V is not a valid target of simple force assignment" },
      { 14, "type of force expression universal_real does not match type "
        "of target INTEGER" },
      { 15, "target of a simple force assignment may not be an aggregate" },
      { 29, "force mode OUT may not be used with target of mode IN" },
      { 30, "target of simple force assignment must be a signal name" },
      { 31, "force expression BOOLEAN does not match type of target BIT" },
      { 39, "variable V is not a valid target of simple release assignment" },
      { 40, "target of a simple release assignment may not be an aggregate" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);

   tree_t s4 = tree_stmt(tree_stmt(a, 0), 4);
   fail_unless(tree_kind(s4) == T_FORCE);
   fail_unless(tree_subkind(s4) == PORT_IN);

   check_expected_errors();
}
END_TEST

START_TEST(test_ename)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/ename.vhd");

   const error_t expect[] = {
      {  2, "sorry, external names in packages are not supported" },
      { 15, "initial value BIT does not match type of declaration INTEGER" },
      { 21, "target of variable assignment must be a variable name or " },
      { 24, "external name Y is not a valid target of simple release " },
      { 25, "generate index must be a static expression" },
      { 14, "cannot reference signal BAR during static elaboration" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_mcase)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/mcase.vhd");

   const error_t expect[] = {
      { 19, "type of expression in a matching case statement must be BIT, "
        "STD_ULOGIC, or a one-dimensional array of these types" },
      { 38, "case expression must have a discrete type or one dimensional "
        "character array type" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_generics2008)
{
   opt_set_int(OPT_WARN_HIDDEN, 1);
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/generics2008.vhd");

   const error_t expect[] = {
      {  5, "no visible declaration for D" },
      {  7, "no visible declaration for E" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY);

   check_expected_errors();
}
END_TEST

START_TEST(test_osvvm5)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/osvvm5.vhd");

   const error_t expect[] = {
      { 48, "cannot use an uninstantiated package" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_check_and_simplify(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_INST,
                            T_PACK_INST, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue465)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/issue465.vhd");

   const error_t expect[] = {
      {  1, "unit WORK.TESTTEST_PKG not found in library WORK" },
      {  8, "design unit depends on WORK.TEST2_PKG which was analysed with" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   lib_put_error(lib_work(), p);

   tree_t b = parse();
   fail_if(b == NULL);
   fail_unless(tree_kind(b) == T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_gensub)
{
   set_standard(STD_08);
   input_from_file(TESTDIR "/sem/gensub.vhd");

   const error_t expect[] = {
      { 21, "function NOT_GENERIC is not an uninstantiated subprogram" },
      { 24, "subprogram NO_BODY cannot be instantiated until its body has "
        "been analysed" },
      { 42, "type of actual universal_integer does not match type T of "
        "formal generic N" },
      { 42, "no visible subprogram \"+\" matches signature [T, T return T]" },
      { 44, "cannot call uninstantiated function ADDER" },
      { 50, "cannot call uninstantiated procedure DO_STUFF" },
      { 57, "no matching operator \"+\" [Q, T return Q]" },
      { 60, "actual associated with generic Y must be a globally static" },
      { 69, "subtype of generic X does not match type INTEGER in spec" },
      { 75, "subprogram TEST2 declaration has 1 generic but body has 2" },
      { 93, "multiple visible subprograms with name TEST1" },
      { 93, "TEST_ERROR has no generic named T" },
      { 94, "no visible subprogram declaration for TEST444" },
      { 94, "TEST_ERROR2 has no generic named T" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue482)
{
   input_from_file(TESTDIR "/sem/issue482.vhd");

   parse_and_check(T_PACKAGE);

   fail_if_errors();
}
END_TEST

START_TEST(test_gensub2)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/gensub2.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_vhdl2019)
{
   set_standard(STD_19);
   input_from_file(TESTDIR "/sem/vhdl2019.vhd");

   const error_t expect[] = {
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_altera1)
{
   input_from_file(TESTDIR "/sem/altera1.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue509)
{
   opt_set_int(OPT_RELAXED, 1);

   input_from_file(TESTDIR "/sem/issue509.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_genpack2)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/genpack2.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACK_INST, T_PACKAGE, T_PACK_BODY,
                   T_PACK_INST, T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue655)
{
   input_from_file(TESTDIR "/sem/issue655.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_if_errors();
}
END_TEST

START_TEST(test_issue659)
{
   input_from_file(TESTDIR "/sem/issue659.vhd");

   const error_t expect[] = {
      {  8, "cannot assign to parameter Y with mode IN" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue660)
{
   input_from_file(TESTDIR "/sem/issue660.vhd");

   const error_t expect[] = {
      {  11, "class (constant) of parameter X of subprogram BLAH not defined equally in subprogram specification and subprogram body" },
      {  16, "mode (IN) of parameter X of subprogram DOSOMETHING not defined equally in subprogram specification and subprogram body" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   check_expected_errors();
}
END_TEST

START_TEST(test_genpack3)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/genpack3.vhd");

   const error_t expect[] = {
      { 21, "WORK.PACK1 has no generic named Z" },
      { 31, "generic X in package WORK.PACK2 does not have a default value" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_PACKAGE);

   check_expected_errors();
}
END_TEST

START_TEST(test_condexpr)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/sem/condexpr.vhd");

   const error_t expect[] = {
      {  7, "type of condition must be BOOLEAN but have universal_integer" },
      {  8, "expected type of conditional expression to be INTEGER "
         "but is universal_real" },
      { 24, "expected return type INTEGER but have universal_real" },
      { 25, "conditional return statement without value is only valid "
        "inside a procedure" },
      { 31, "type of condition must be BOOLEAN but have INTEGER" },
      { 34, "return statement not allowed outside subprogram" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_lcs2016_72b)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/sem/lcs2016_72b.vhd");

   const error_t expect[] = {
      { 37, "function IOTA with return identifier RESULT_T cannot be called" },
      { 40, "type of value RESULT_T does not match type of target INTEGER" },
      { 41, "function IOTA with return identifier RESULT_T cannot be called "
        "in this context as the result subtype is not known" },
      { 42, "function IOTA with return identifier RESULT_T cannot be "},
      { 47, "type mark of return identifier must denote an unconstrained " },
      { 55, "type mark of return identifier must denote an unconstrained " },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_lcs2016_47)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/sem/lcs2016_47.vhd");

   const error_t expect[] = {
      { 35, "formal variable port X must have mode INOUT" },
      { 36, "formal variable port Y must have protected type" },
      { 44, "actual associated with port X of mode OUT" },
      { 44, "formal variable port Y must either be a shared variable "
        "or a formal variable port of another design entity" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_lcs2016_45a)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/sem/lcs2016_45a.vhd");

   const error_t expect[] = {
      { 20, "record type REC_T has no field named X" },
      { 23, "missing mode view element defintion for B, C" },
      { 27, "subtype indication of a mode view declaration must denote "
        "a record type" },
      { 37, "subtype indication of a mode view declaration must denote an "
        "unresolved record type" },
      { 42, "record type REC_T has no field named INTEGER" },
      { 48, "duplicate mode view element definition for field B" },
      { 51, "type mark MASTER1 does not denote a type or a subtype" },
      { 53, "prefix of 'CONVERSE attribute must be a named mode view or " },
      { 64, "name in mode view indication does not denote a mode view" },
      { 65, "subtype INTEGER is not compatible with mode view MASTER1" },
      { 74, "element mode indication cannot have mode LINKAGE" },
      { 88, "name in element mode view indication of field R does not " },
      { 94, "port Y with array mode view indication has non-array type REC_T" },
      { 95, "subtype BIT is not compatible with mode view MASTER1" },
      { 105, "R with array mode view indication has non-array type REC_T" },
      { 109, "S subtype REC_VEC_T is not compatible with mode view MASTER1" },
      { 157, "cannot assign to element B of port R which has mode IN from "
        "mode view indication" },
      { 158, "cannot assign to port R with mode view indication as one or "
        "more sub-elements have mode IN" },
      { 160, "cannot assign to element B of port A which has mode IN from "
        "mode view indication" },
      { 165, "mode view indication of formal port P element A is not "
        "compatible with actual" },
      { 167, "actual associated with port P with mode view indication must "
        "be a static signal name" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_lcs2016_18)
{
   set_standard(STD_19);

   input_from_file(TESTDIR "/sem/lcs2016_18.vhd");

   const error_t expect[] = {
      {  7, "prefix of 'DESIGNATED_SUBTYPE attribute does not have a type" },
      {  8, "prefix of 'DESIGNATED_SUBTYPE attribute must be an access "
         "or file type" },
      { 15, "type of initial value BOOLEAN does not match type of "
        "declaration INTEGER" },
      { 23, "type of initial value universal_integer does not match type of "
        "declaration BOOLEAN" },
      { 24, "'INDEX parameter for type INT_MAP must be between 1 and 2" },
      { 25, "'INDEX parameter for type INT_MAP must be between 1 and 2" },
      { 26, "prefix of 'INDEX attribute must be an array type" },
      { 29, "only integer literals are supported for 'INDEX parameter" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   check_expected_errors();
}
END_TEST

START_TEST(test_issue713)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/issue713.vhd");

   const error_t expect[] = {
      { 17, "invalid use of type A" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACK_INST);

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
   tcase_add_test(tc_core, test_gentype);
   tcase_add_test(tc_core, test_record2008);
   tcase_add_test(tc_core, test_osvvm4);
   tcase_add_test(tc_core, test_agg2008);
   tcase_add_test(tc_core, test_force);
   tcase_add_test(tc_core, test_ename);
   tcase_add_test(tc_core, test_mcase);
   tcase_add_test(tc_core, test_generics2008);
   tcase_add_test(tc_core, test_osvvm5);
   tcase_add_test(tc_core, test_issue465);
   tcase_add_test(tc_core, test_gensub);
   tcase_add_test(tc_core, test_issue482);
   tcase_add_test(tc_core, test_gensub2);
   tcase_add_test(tc_core, test_vhdl2019);
   tcase_add_test(tc_core, test_altera1);
   tcase_add_test(tc_core, test_issue509);
   tcase_add_test(tc_core, test_genpack2);
   tcase_add_test(tc_core, test_issue655);
   tcase_add_test(tc_core, test_issue659);
   tcase_add_test(tc_core, test_issue660);
   tcase_add_test(tc_core, test_genpack3);
   tcase_add_test(tc_core, test_condexpr);
   tcase_add_test(tc_core, test_lcs2016_72b);
   tcase_add_test(tc_core, test_lcs2016_47);
   tcase_add_test(tc_core, test_lcs2016_45a);
   tcase_add_test(tc_core, test_lcs2016_18);
   tcase_add_test(tc_core, test_issue713);
   suite_add_tcase(s, tc_core);

   return s;
}
