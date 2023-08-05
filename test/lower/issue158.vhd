package cannot_return_safety_check_pkg is
    impure function fun return string;
end package cannot_return_safety_check_pkg;

package body cannot_return_safety_check_pkg is
  type string_ptr is access string;
  procedure proc(variable value : inout string_ptr) is
  begin
    value := new string'("buggy");
  end procedure;

  impure function fun return string is
    variable value : string_ptr;
  begin
    proc(value);
    return value.all;      -- Could not return safety check
  end;
end package body;
