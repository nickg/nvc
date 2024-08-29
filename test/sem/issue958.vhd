package foo is
  subtype bar is bit_vector;

  function "-"(arg: in bar) return bar;
  function "+"(lhs, rhs: in bar) return bar;
  function "*"(lhs, rhs: in bar) return bar;
end foo;

package body foo is
  function "-"(arg: bar) return bar is  -- Error
  begin
  end function "-";

  function "+"(lhs, rhs: bar) return bar is  -- Error (once)
  begin
  end function "+";

  function "*"(lhs : in bar; rhs : in bar) return bar is  -- Error
  begin
  end function;
end foo;
