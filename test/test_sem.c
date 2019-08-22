#include "type.h"
#include "phase.h"
#include "util.h"
#include "common.h"
#include "test_util.h"

#include <check.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

START_TEST(test_integer)
{
   tree_t a, d, p, s, e;
   type_t t;

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
      { 35, "type NOTHING is not declared" },
      { 48, "no suitable overload for operator \"*\" [MY_INT2, MY_INT1" },
      { 57, "MY_INT2 has no attribute CAKE" },
      { -1, NULL }
   };
   expect_errors(expect);

   sem_check(a);
   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);

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
}
END_TEST

START_TEST(test_ports)
{
   input_from_file(TESTDIR "/sem/ports.vhd");

   const error_t expect[] = {
      { 31,  "cannot read output port O" },
      { 42,  "cannot assign to input port I" },
      { 81,  "missing actual for formal I of mode IN without a default expression" },
      { 85,  "formal I already has an actual" },
      { 89,  "too many positional actuals" },
      { 89,  "too many positional actuals" },
      { 92,  "no visible declaration for CAKE" },
      { 94,  "cannot find unit WORK.BAD" },
      { 103, "unconnected port I with mode IN must have a default value" },
      { 116, "object X is not a component declaration" },
      { 148, "actual must be globally static expression" },
      { 155, "no visible declaration for Q" },
      { 163, "no visible declaration for U" },
      { 168, "formal name must be locally static" },
      { 177, "formal name must be locally static" },
      { 185, "no visible declaration for HELLO" },
      { 217, "port O of unconstrained type INT_VEC cannot be unconnected" },
      { 221, "type of actual universal real does not match type INTEGER" },
      { 271, "result of conversion for unconstrained formal I must" },
      { 279, "actual must be globally static expression or locally" },
      { 283, "conversion not allowed for formal O with mode OUT" },
      { 293, "output conversion not allowed for formal I with mode IN" },
      { 297, "output conversion for formal B must not have OPEN actual" },
      { 311, "cannot assign to input port X" },
      { 312, "cannot read output port Y" },
      { 332, "cannot read parameter X with mode IN" },
      { 355, "actual must be globally static expression or locally static" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ARCH,
                   T_ARCH, T_ENTITY, T_ARCH, T_ARCH, T_ARCH);

   fail_unless(parse() == NULL);
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST


START_TEST(test_scope)
{
   input_from_file(TESTDIR "/sem/scope.vhd");

   const error_t expect[] = {
      {  31, "WORK.PACK1.MY_INT1 does not match type"
         " of target WORK.PACK2.MY_INT1" },
      {  44, "WORK.PACK1.MY_INT1 does not match type of target MY_INT1" },
      {  63, "G already declared in this region" },
      {  54, "previous declaration of G was here" },
      {  71, "P already declared in this region" },
      {  55, "previous declaration of P was here" },
      { 114, "no visible declaration for MY_INT1" },
      { 137, "no visible declaration for E1" },
      { 160, "no visible declaration for FUNC2" },
      { 167, "declaration NOT_HERE not found in unit WORK.PACK5" },
      { 189, "no visible declaration for MY_INT1" },
      { 236, "missing library clause for FOO" },
      { 306, "no visible declaration for L1.X" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH, T_ENTITY,
                   T_ARCH, T_ARCH, T_ARCH, T_ENTITY, T_ARCH, T_ENTITY,
                   T_PACKAGE, T_PACKAGE, T_ARCH, T_PACKAGE, T_ARCH,
                   T_ARCH, T_ENTITY, T_ARCH, T_ARCH, T_PACKAGE, T_ARCH,
                   T_ARCH, T_PACKAGE, T_ARCH, T_ARCH, T_PACKAGE, T_ARCH,
                   T_ARCH, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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
      { 141, "ambiguous use of operator \"<\"" },
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

   parse_and_check(T_PACKAGE, T_PACKAGE, T_ARCH, T_ARCH, T_ARCH,
                   T_ARCH, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_const)
{
   input_from_file(TESTDIR "/sem/const.vhd");

   const error_t expect[] = {
      { 24, "invalid target of variable assignment" },
      { 28, "deferred constant declarations are only permitted" },
      { 58, "constant WORK.P.C already has a value" },
      { 59, "expected type INTEGER for deferred constant WORK.P.F" },
      { 49, "deferred constant WORK.P.D was not given a value" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_const2)
{
   input_from_file(TESTDIR "/sem/const2.vhd");

   tree_t p = parse();
   fail_if(p == NULL);
   fail_unless(tree_kind(p) == T_PACKAGE);
   sem_check(p);

   fail_unless(sem_errors() == 0);

   fail_unless(tree_kind(p) == T_PACKAGE);

   // The deferred constant array's type is originally unconstrained
   tree_t d = tree_decl(p, 1);
   fail_unless(tree_kind(d) == T_CONST_DECL);
   fail_unless(type_is_unconstrained(tree_type(d)));

   tree_t pb = parse();
   fail_if(pb == NULL);
   fail_unless(tree_kind(pb) == T_PACK_BODY);
   sem_check(pb);

   fail_unless(sem_errors() == 0);

   // and then gets constrained after analysing the package body
   fail_unless(!type_is_unconstrained(tree_type(d)));
}
END_TEST

START_TEST(test_std)
{
   input_from_file(TESTDIR "/sem/std.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
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
   fail_unless(parse_errors() == 0);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_func)
{
   input_from_file(TESTDIR "/sem/func.vhd");

   const error_t expect[] = {
      {   5, "function arguments must have mode IN" },
      {  17, "must be an unconstrained array type" },
      {  21, "resolution function must have single argument" },
      {  25, "declaration UENUM is not a function" },
      {  27, "type of default value universal integer does not" },
      {  29, "subprogram body is not allowed in package specification" },
      {  36, "missing declaration for package BAD" },
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
      { 239, "class variable of subprogram body WORK.FUNC2.TEST25 parameter" },
      { 234, "parameter X was originally declared here" },
      { 245, "class default of subprogram body WORK.FUNC2.TEST26 parameter" },
      { 243, "parameter X was originally declared here" },
      { 271, "invalid reference to X inside pure function NESTED" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACK_BODY, T_PACKAGE,
                   T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_array)
{
   input_from_file(TESTDIR "/sem/array.vhd");

   const error_t expect[] = {
      { 27, "positional associations must appear first in aggregate" },
      { 33, "named association must not follow others" },
      { 39, "only a single others association allowed" },
      { 46, "type of initial value universal integer does not match" },
      { 55, "type of value universal integer does not match type of" },
      { 57, "type of value INT_ARRAY does not match type" },
      { 65, "for operator \"=\" [INT_ARRAY, TEN_INTS return BOOLEAN]" },
      { 88, "array W has 2 dimensions but 1 indices given" },
      { 89, "array W has 2 dimensions but 3 indices given" },
      { 98, "type of index universal integer does not match type" },
      { 102, "named and positional associations cannot be mixed in" },
      { 111, "non-locally static choice must be only choice" },
      { 119, "type of slice prefix is not an array" },
      { 120, "range direction of slice TO does not match prefix DOWNTO" },
      { 121, "others choice not allowed in this context" },
      { 130, "range direction of slice DOWNTO does not match prefix TO" },
      { 207, "array BAD cannot have unconstrained element type" },
      { 215, "others choice not allowed in this context" },
      { 232, "aliased name is not static" },
      { 233, "aliased name is not static" },
      { 234, "type of aliased object INT_ARRAY does not match" },
      { 241, "no visible declaration for I" },
      { 246, "universal integer bound must be numeric literal or attribute" },
      { 252, "expected 1 constraints for type INT_ARRAY but found 2" },
      { 272, "no suitable overload for operator \"<\" [INT2D, INT2D" },
      { 277, "type NOT_HERE is not declared" },
      { 279, "type NUM_ARRAY is unconstrained" },
      { 285, "object K does not have a range" },
      { 295, "type of index universal integer does not match" },
      { 343, "invalid character 'f' in string literal of type BIT_VECTOR" },
      { 365, "may not change constraints of a constrained array" },
      { 366, "may not change constraints of a constrained array" },
      { 373, "too many elements in array" },
      { 379, "array NO_FILE_TYPES.T_FILE_ARRAY cannot have element of file" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_generics)
{
   input_from_file(TESTDIR "/sem/generics.vhd");

   const error_t expect[] = {
      {  34, "missing actual for formal N" },
      {  38, "too many positional actuals" },
      {  48, "no visible declaration for X" },
      {  58, "invalid object class for generic" },
      {  68, "no visible declaration for Y" },
      { 109, "actual must be globally static expression or locally " },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ENTITY, T_ENTITY,
                   T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_seq)
{
   input_from_file(TESTDIR "/sem/seq.vhd");

   const error_t expect[] = {
      {  15, "type of test must be BOOLEAN" },
      {  19, "no visible declaration for X" },
      {  25, "name TRUE cannot be used in this context" },
      {  32, "no visible declaration for X" },
      {  48, "return statement not allowed outside subprogram" },
      {  62, "return statement not allowed outside subprogram" },
      {  64, "type of loop condition must be BOOLEAN" },
      {  79, "no visible declaration for X" },
      { 106, "others choice must appear last" },
      { 113, "case choice must be locally static" },
      { 126, "case choice must be locally static" },
      { 136, "case choice must be locally static" },
      { 139, "invalid use of type BIT" },
      { 146, "type mismatch in range" },
      { 150, "expected type of range bound to be STD.STANDARD.INTEGER but is" },
      { 154, "right index of case choice range is not locally static" },
      { 164, "type of exit condition must be BOOLEAN" },
      { 179, "actual for formal Y with class VARIABLE must be" },
      { 187, "type of next condition must be BOOLEAN" },
      { 190, "cannot use next outside loop" },
      { 192, "no nested loop with label FOO" },
      { 205, "DUP already declared in this region" },
      { 204, "previous declaration of DUP was here" },
      { 214, "type REAL does not have a range" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_conc)
{
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

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_procedure)
{
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
      { 148, "target of signal assignment is not a signal" },
      { 157, "object ARG with type containing an access type must have class VARIABLE" },
      { 162, "object ARG with type containing an access type must have class VARIABLE" },
      { 167, "object ARG with type containing an access type must have class VARIABLE" },
      { 172, "object ARG with type containing an access type must have class VARIABLE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_concat)
{
   input_from_file(TESTDIR "/sem/concat.vhd");

   const error_t expect[] = {
      { 24, "type of scalar does not match element type of array" },
      { 26, "cannot concatenate values of different types" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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
      { 127, "cannot index non-array type universal integer" },
      { 133, "class of object I is variable not signal" },
      { 140, "prefix of attribute LAST_EVENT must denote a signal" },
      { 152, "attribute RANGE with unconstrained array type BIT_VECTOR" },
      { 153, "object B does not have a range" },
      { 154, "prefix does not have a range" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH, T_ARCH, T_PACKAGE, T_PACK_BODY,
                   T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_generate)
{
   input_from_file(TESTDIR "/sem/generate.vhd");

   const error_t expect[] = {
      { 15, "condition of generate statement must be BOOLEAN" },
      { 26, "no visible declaration for Y" },
      { 45, "condition of generate statement must be static" },
      { 48, "range of generate statement must be static" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_record)
{
   input_from_file(TESTDIR "/sem/record.vhd");

   const error_t expect[] = {
      {   9, "duplicate field name X" },
      {  15, "recursive record types are not allowed" },
      {  30, "field X with unconstrained array type is not allowed" },
      {  39, "field Z does not have a value" },
      {  40, "does not match type of field Y" },
      {  42, "field Y does not have a value" },
      {  43, "type R1 does not have field named Q" },
      {  44, "type of value R1 does not match type of" },
      {  47, "field X already has a value" },
      {  48, "field X already has a value" },
      {  64, "type R1_VEC is unconstrained" },
      {  72, "record type R1 has no field F" },
      {  82, "record type R1_SUB has no field Z" },
      {  86, "record subtype may not have constraints" },
      { 106, "record type R1 has no field Z" },
      { 111, "record field A cannot be of file type" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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
      { 46, "type WORK.P.T_PTR_ARR has a subelement with an access type" },
      { 47, "type WORK.P.SUB_PTR_ARR has a subelement with an access type" },
      { 48, "type WORK.P.T_REC has a subelement with an access type" },
      { 49, "type WORK.P.T_REC2 has a subelement with an access type" },
      { 74, "no suitable overload for procedure READ" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_access)
{
   input_from_file(TESTDIR "/sem/access.vhd");

   const error_t expect[] = {
      {  5, "no visible declaration for FOO" },
      { 34, "null expression must have access type" },
      { 38, "invalid allocator expression" },
      { 39, "name I does not refer to a type" },
      { 41, "does not match type of target INT_PTR" },
      { 47, "type of value REC does not match type of" },
      { 55, "type of allocator expresion INTEGER does not match" },
      { 56, "name S does not refer to a type" },
      { 76, "unconstrained array type INT_PTR_ARRAY not allowed" },
      { 84, "non-array type INTEGER may not have index constraint" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_real)
{
   input_from_file(TESTDIR "/sem/real.vhd");

   const error_t expect[] = {
      { 16, "type of value MY_REAL does not match type of target" },
      { 25, "conversion only allowed between closely related types" },
      { 38, "type of left bound INTEGER does not match type of right" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_entity)
{
   input_from_file(TESTDIR "/sem/entity.vhd");

   const error_t expect[] = {
      { 26, "unit WORK.PACK is not an entity" },
      { 30, "unit WORK.PACK is not an entity" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_PACKAGE, T_ARCH, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_signal)
{
   input_from_file(TESTDIR "/sem/signal.vhd");

   const error_t expect[] = {
      { 14, "no composite type in context" },
      { 15, "no one dimensional arrays of character type in context" },
      { 16, "not a suitable l-value" },
      { 17, "others association not allowed in aggregate signal target" },
      { 18, "cannot assign to input port P" },
      { 22, "no composite type in context" },
      { 23, "not a suitable l-value" },
      { 24, "others association not allowed in aggregate signal target" },
      { 25, "cannot assign to input port P" },
      { 30, "aggregate element must be locally static name" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_static)
{
   input_from_file(TESTDIR "/sem/static.vhd");

   const error_t expect[] = {
      { 36, "case choice must be locally static" },
      { 42, "case choice must be locally static" },
      { 65, "actual must be globally static expression or locally static" },
      { 85, "formal name must be locally static" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_subtype)
{
   input_from_file(TESTDIR "/sem/subtype.vhd");

   const error_t expect[] = {
      {  9, "expected type of range bound to be STD.STANDARD.INTEGER but is" },
      { 16, "undefined resolution function NOT_HERE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_universal)
{
   input_from_file(TESTDIR "/sem/universal.vhd");

   const error_t expect[] = {
      { 12, "no suitable overload for operator \"*\" [REAL" },
      { 14, "no suitable overload for operator \"*\" [INTEGER" },
      { 16, "no suitable overload for operator \"/\" [universal real" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue52)
{
   input_from_file(TESTDIR "/sem/issue52.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue58)
{
   input_from_file(TESTDIR "/sem/issue58.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_spec)
{
   input_from_file(TESTDIR "/sem/spec.vhd");

   const error_t expect[] = {
      { 22, "component mismatch for instance I1: expected C1" },
      { 24, "object E is not a component declaration" },
      { 28, "instance BAD not found" },
      { 30, "specification may only be used with component instances" },
      { 32, "instance I1 is already bound by a specification" },
      { 34, "cannot find unit WORK.NOT_HERE" },
      { 36, "unit WORK.P is not an entity" },
      { 40, "instance I5 not found" },
      { 42, "instance I4 is already bound by a specification" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue53)
{
   input_from_file(TESTDIR "/sem/issue53.vhd");

   parse_and_check(T_ENTITY);

   fail_unless(sem_errors() == 0);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_config)
{
   input_from_file(TESTDIR "/sem/config.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH, T_ENTITY,
                   T_ARCH, T_CONFIGURATION);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_protected)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/protected.vhd");

   const error_t expect[] = {
      {  13, "no visible declaration for NOT_HERE" },
      {  19, "no protected type declaration for BAD2 found" },
      {  22, "object INTEGER is not a protected type declaration" },
      {  25, "object NOW is not a protected type declaration" },
      {  47, "protected type SHAREDCOUNTER already has body" },
      {  50, "subtypes may not have protected base types" },
      {  52, "shared variable X must have protected type" },
      {  56, "variable Y with protected type may not have an initial value" },
      {  64, "parameter with protected type can not have a default value" },
      { 118, "no visible declaration for X.COUNTER" },
      { 119, "no suitable overload for procedure X.DECREMENT" },
      { 124, "object X with protected type must have class VARIABLE" },
      { 135, "may not assign to variable of a protected type" },
      { 150, "missing body for protected type WORK.PKG.PROTECTED_T" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH, T_PACKAGE, T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_alias)
{
   input_from_file(TESTDIR "/sem/alias.vhd");

   const error_t expect[] = {
      { 10, "non-object alias may not have subtype indication" },
      { 12, "name X does not refer to a type" },
      { 22, "no visible subprogram FOO matches signature [INTEGER "
        "return INTEGER]" },
      { 23, "no visible subprogram FOO matches signature [BIT]" },
      { 24, "invalid name in subprogram alias" },
      { 25, "no visible declaration for BLAH" },
      { 32, "no visible subprogram BAR matches signature [INTEGER]" },
      { 40, "ambiguous use of enumeration literal '1'" },
      { 41, "no visible declaration for FOO_INT" },
      { 42, "no suitable overload for procedure BAR_BIT [CHARACTER]" },
      { 49, "aliased name is not static" },
      { -1, NULL }
   };
   expect_errors(expect);

   tree_t arch = parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);

   tree_t x_decl = tree_decl(arch, 1);
   fail_unless(tree_kind(x_decl) == T_SIGNAL_DECL);
   fail_unless(icmp(tree_ident(x_decl), "X"));
   fail_unless(tree_flags(x_decl) & TREE_F_LAST_VALUE);
}
END_TEST

START_TEST(test_issue102)
{
   input_from_file(TESTDIR "/sem/issue102.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue105)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/issue105.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue88)
{
   input_from_file(TESTDIR "/sem/issue88.vhd");

   const error_t expect[] = {
      { 31, "record type REC2 has no field P" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue128)
{
   input_from_file(TESTDIR "/sem/issue128.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue130)
{
   input_from_file(TESTDIR "/sem/issue130.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue132)
{
   input_from_file(TESTDIR "/sem/issue132.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue131)
{
   input_from_file(TESTDIR "/sem/issue131.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue133)
{
   input_from_file(TESTDIR "/sem/issue133.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue140)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/sem/issue140.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH,
                   T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue144)
{
   input_from_file(TESTDIR "/sem/issue144.vhd");

   const error_t expect[] = {
      { 11, "duplicate subprogram body for function FUN [return INTEGER]" },
      { 20, "duplicate subprogram body for procedure PROC [INTEGER]" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue151)
{
   input_from_file(TESTDIR "/sem/issue151.vhd");

   parse_and_check(T_PACKAGE, T_PACKAGE, T_PACKAGE);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_duplicate)
{
   input_from_file(TESTDIR "/sem/duplicate.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue165)
{
   input_from_file(TESTDIR "/sem/issue165.vhd");

   const error_t expect[] = {
      {  5, "no visible declaration for TYPE_T" },
      { 11, "no suitable overload for procedure PROC [universal integer]" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue162)
{
   input_from_file(TESTDIR "/sem/issue162.vhd");

   tree_t body = parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);

   fail_unless(tree_kind(body) == T_PACK_BODY);

   tree_t p = tree_decl(body, 2);
   fail_unless(tree_kind(p) == T_PROC_BODY);
   fail_unless(icmp(tree_ident(p), "WORK.AMBIGUOUS.CALLING_PROC"));
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 0)))->first_line == 10);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 1)))->first_line == 10);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 2)))->first_line == 5);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 3)))->first_line == 5);
   fail_unless(tree_loc(tree_ref(tree_stmt(p, 4)))->first_line == 10);

   tree_t f1 = tree_decl(body, 5);
   fail_unless(tree_kind(f1) == T_FUNC_BODY);
   fail_unless(icmp(tree_ident(f1), "WORK.AMBIGUOUS.CALLING_FUN_WORKS"));

   tree_t c0 = tree_value(tree_param(tree_value(tree_stmt(f1, 0)), 0));
   fail_unless(tree_loc(tree_ref(c0))->first_line == 29);

   tree_t c1 = tree_value(tree_param(tree_value(tree_stmt(f1, 1)), 0));
   fail_unless(tree_loc(tree_ref(c1))->first_line == 23);

   tree_t c2 = tree_value(tree_param(tree_value(tree_stmt(f1, 2)), 0));
   fail_unless(tree_loc(tree_ref(c2))->first_line == 23);

   tree_t f2 = tree_decl(body, 6);
   fail_unless(tree_kind(f2) == T_FUNC_BODY);
   fail_unless(icmp(tree_ident(f2), "WORK.AMBIGUOUS.CALLING_FUN"));

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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_afunc)
{
   input_from_file(TESTDIR "/sem/afunc.vhd");

   tree_t a = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

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

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue174)
{
   input_from_file(TESTDIR "/sem/issue174.vhd");

   lib_t other = lib_tmp("lib");
   lib_set_work(other);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_varinit)
{
   input_from_file(TESTDIR "/sem/varinit.vhd");

   const error_t expect[] = {
      { 26, "cannot reference signal SIZE during static elaboration" },
      { 37, "cannot reference signal SIZE during static elaboration" },
      { 39, "cannot reference signal SIZE during static elaboration" },
      { 47, "cannot reference signal N during static elaboration" },
      { 48, "cannot reference signal N during static elaboration" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue201)
{
   set_standard(STD_00);

   input_from_file(TESTDIR "/sem/issue201.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue176)
{
   input_from_file(TESTDIR "/sem/issue176.vhd");

   const error_t expect[] = {
      { 36, "function MAIN.FUN cannot call procedure MAIN.PROC_WAIT" },
      { 37, "function MAIN.FUN cannot call procedure MAIN.PROC_WAIT which" },
      { 51, "function FUN2 cannot call procedure WORK.PACK.PROC" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_context)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/context.vhd");

   const error_t expect[] = {
      { 26, "unit FOO.PACK is not a context declaration" },
      { 39, "library clause in a context declaration may not have logical" },
      { 40, "context declaration use clause may not have WORK" },
      { 41, "context declaration context reference may not have WORK" },
      { -1, NULL }
   };
   expect_errors(expect);

   lib_t foo = lib_tmp("foo");
   lib_t bar = lib_tmp("bar");

   lib_set_work(foo);
   parse_and_check(T_PACKAGE, T_CONTEXT, -1);

   lib_set_work(bar);
   parse_and_check(T_ENTITY, T_ENTITY, T_PACKAGE, T_CONTEXT);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue89)
{
   input_from_file(TESTDIR "/sem/issue89.vhd");

   tree_t e = parse_and_check(T_ENTITY, T_ARCH);
   fail_unless(sem_errors() == 0);

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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue219)
{
   input_from_file(TESTDIR "/sem/issue219.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue220)
{
   input_from_file(TESTDIR "/sem/issue220.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue224)
{
   input_from_file(TESTDIR "/sem/issue224.vhd");

   const error_t expect[] = {
      {  6, "parameter of class SIGNAL can not have a default value" },
      { 18, "actual for formal A with class SIGNAL must not be OPEN" },
      { 24, "parameter of class VARIABLE with mode OUT or INOUT can not have a default value" },
      { 34, "parameter of class VARIABLE with mode OUT or INOUT can not have a default value" },
      { 43, "port with mode LINKAGE can not have a default value" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH, T_ARCH, T_ARCH, T_ARCH, T_ENTITY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue221)
{
   set_standard(STD_08);

   input_from_file(TESTDIR "/sem/issue221.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue236)
{
   input_from_file(TESTDIR "/sem/issue236.vhd");

   const error_t expect[] = {
      { 24,  "missing actual for formal B of mode IN without a default expression" },
      { 36,  "missing actual for formal C with unconstrained array type" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ENTITY, T_ARCH, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue264)
{
   input_from_file(TESTDIR "/sem/issue264.vhd");

   const error_t expect[] = {
      { 23, "no visible one dimensional array type with element INTEGER" },
      { 26, "result of concatenation is ambiguous" },
      { 35, "case expression must have a discrete type or one dimensional" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue226)
{
   input_from_file(TESTDIR "/sem/issue226.vhd");

   const error_t expect[] = {
      { 14, "no visible declaration for IEEE in name IEEE.STD_LOGIC_" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue197)
{
   input_from_file(TESTDIR "/sem/issue197.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue276)
{
   set_standard(STD_02);

   input_from_file(TESTDIR "/sem/issue276.vhd");

   lib_t foo = lib_tmp("foo");
   lib_set_work(foo);

   parse_and_check(T_PACKAGE, T_PACK_BODY, T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_dwlau)
{
   input_from_file(TESTDIR "/sem/dwlau.vhd");

   const error_t expect[] = {
      { 20, "no suitable overload for operator \"*\"" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_jcore1)
{
   input_from_file(TESTDIR "/sem/jcore1.vhd");

   const error_t expect[] = {
      { 15, "no visible declaration for STD_LOGIC" },
      { 25, "no visible declaration for STD_LOGIC" },
      { 45, "no visible declaration for PIPELINE_EX_T" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue293)
{
   input_from_file(TESTDIR "/sem/issue293.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue311)
{
   input_from_file(TESTDIR "/sem/issue311.vhd");

   const error_t expect[] = {
      { 33, "name P2.EVENT_1 cannot be used in this context" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH,
                   T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_foreign1)
{
   input_from_file(TESTDIR "/sem/foreign1.vhd");

   tree_t body = parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);

   tree_t decl = tree_decl(body, 0);
   fail_unless(icmp(tree_ident(decl), "WORK.P.PROC"));
   fail_unless(tree_attr_int(decl, wait_level_i, WAITS_MAYBE) == WAITS_NO);
}
END_TEST

START_TEST(test_issue316)
{
   input_from_file(TESTDIR "/sem/issue316.vhd");

   const error_t expect[] = {
      { 30, "actual for formal REG_IN with class SIGNAL must be a name" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue350)
{
   input_from_file(TESTDIR "/sem/issue350.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue359a)
{
   input_from_file(TESTDIR "/sem/issue359a.vhd");

   const error_t expect[] = {
      { 15, "invalid use of procedure HOST_WRITE" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue368)
{
   input_from_file(TESTDIR "/sem/issue368.vhd");

   const error_t expect[] = {
      { 17, "with mode IN does not match mode OUT in specification" },
      {  6, "parameter SIG_IN was originally declared here" },
      { 18, "parameter SIG_OUT of subprogram body WORK.NVC_PACKAGE_BUG" },
      {  7, "parameter SIG_OUT was originally declared here" },
      { 23, "subprogram body WORK.NVC_PACKAGE_BUG.FOO missing parameter X" },
      { 10, "parameter X was originally declared here" },
      { 27, "class default of subprogram body WORK.NVC_PACKAGE_BUG.BAR" },
      { 11, "parameter X was originally declared here" },
      { 31, "subprogram body WORK.NVC_PACKAGE_BUG.BAZ missing parameter X" },
      { 12, "parameter X was originally declared here" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue363)
{
   input_from_file(TESTDIR "/sem/issue363.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_vests1)
{
   input_from_file(TESTDIR "/sem/vests1.vhd");

   parse_and_check(T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue369)
{
   input_from_file(TESTDIR "/sem/issue369.vhd");

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue326)
{
   input_from_file(TESTDIR "/sem/issue326.vhd");

   parse_and_check(T_PACKAGE, T_PACK_BODY);

   fail_unless(sem_errors() == 0);
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

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue341)
{
   input_from_file(TESTDIR "/sem/issue341.vhd");

   const error_t expect[] = {
      { 23, "name 'y' cannot be used in this context (BIT)" },
      { -1, NULL }
   };
   expect_errors(expect);

   parse_and_check(T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
}
END_TEST

START_TEST(test_issue340)
{
   input_from_file(TESTDIR "/sem/issue340.vhd");

   parse_and_check(T_ENTITY, T_ARCH, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);
}
END_TEST

START_TEST(test_issue225)
{
   input_from_file(TESTDIR "/sem/issue225.vhd");

   tree_t a = parse_and_check(T_PACKAGE, T_PACKAGE, T_PACKAGE, T_PACKAGE,
                              T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH);

   fail_unless(sem_errors() == 0);

   fail_unless(tree_contexts(a) == 7);
   fail_unless(tree_ident(tree_context(a, 2)) == ident_new("WORK.P2"));
   fail_unless(tree_ident(tree_context(a, 3)) == ident_new("WORK.P3"));
   fail_unless(tree_ident(tree_context(a, 4)) == ident_new("WORK.P4"));
   fail_unless(tree_ident(tree_context(a, 5)) == ident_new("WORK.P5"));
   fail_unless(tree_ident(tree_context(a, 6)) == ident_new("WORK.P6"));

   tree_t e = tree_ref(a);
   fail_unless(tree_contexts(e) == 3);
   fail_unless(tree_ident(tree_context(e, 2)) == ident_new("WORK.P1"));
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

   set_relax_rules(RELAX_PREFER_EXPLICT);

   input_from_file(TESTDIR "/sem/issue377.vhd");

   tree_t a1 = parse_and_check(T_PACKAGE, T_PACKAGE, T_ENTITY, T_ARCH, -1);

   {
      tree_t fcall = tree_value(tree_stmt(a1, 0));
      fail_unless(tree_kind(fcall) == T_FCALL);

      tree_t decl = tree_ref(fcall);
      fail_unless(tree_attr_str(decl, builtin_i) == NULL);
      fail_unless(tree_ident(decl) == ident_new("WORK.P2.\"=\""));
   }

   tree_t a2 = parse_and_check(T_ARCH);

   {
      tree_t fcall = tree_value(tree_stmt(a2, 0));
      fail_unless(tree_kind(fcall) == T_FCALL);

      tree_t decl = tree_ref(fcall);
      fail_unless(tree_attr_str(decl, builtin_i) == NULL);
      fail_unless(tree_ident(decl) == ident_new("WORK.P2.\"=\""));
   }

   fail_unless(sem_errors() == ARRAY_LEN(expect) - 1);
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
   suite_add_tcase(s, tc_core);

   return s;
}
