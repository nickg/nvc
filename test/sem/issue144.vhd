package multiple_function_bodies_pkg is
  function fun return integer;
end package;

package body multiple_function_bodies_pkg is
  function fun return integer is
  begin
    return 0;
  end function;

  function fun return integer is        -- Error
  begin
    return 1;
  end function;

  procedure proc(x : integer) is
  begin
  end procedure;

  procedure proc(x : integer) is        -- Error
  begin
  end procedure;
end package body;
