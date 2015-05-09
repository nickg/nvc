package ambiguous is
end package;

package body ambiguous is
  procedure proc(arg1 : integer;
                 arg2 : boolean := false) is
  begin
  end procedure;

  procedure proc(arg2 : boolean := false) is
  begin
  end procedure;

  procedure calling_proc is
  begin
    proc; -- Works
    proc(false); -- Works
    proc(1, false); -- Works
    proc(arg1 => 1, arg2 => true); -- Works
    proc(arg2 => true); -- Adding the named argument cause error on ambiguous call
  end procedure;

  function fun(arg1 : integer;
               arg2 : boolean := false) return integer is
  begin
    return 1;
  end function;

  function fun(arg2 : boolean := false) return integer is
  begin
    return 0;
  end function;

  function calling_fun_works(arg2 : boolean := false) return integer is
  begin
      -- Works
      assert fun(true) = 0;
      assert fun(1, true) = 0;
      assert fun(arg1 => 1, arg2 => true) = 0;
      return 0;
  end function;

  function calling_fun(arg2 : boolean := false) return integer is
  begin
    return fun(arg2 => true); -- Adding named argument cause error on ambiguous call
  end function;
end package body;
