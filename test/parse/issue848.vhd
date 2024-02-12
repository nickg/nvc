package test_pkg is
  shared variable v_test : natural;
  impure function func return natural;
  procedure proc(var : out natural);
  pure function test return natural;
  procedure proc2(var : out natural);
  pure function test2 return natural;
  procedure proc3(var : out natural);
  pure function test3 return natural;

end package test_pkg;

package body test_pkg is
  impure function func return natural is
  begin
    return v_test;
  end function;

  procedure proc(var : out natural) is
  begin
    var := func;
  end procedure;

  procedure proc2(var : out natural) is
  begin
    var := v_test;
  end procedure;

  procedure proc3(var : out natural) is
  begin
    proc2(var);
  end procedure;

  pure function test return natural is
    variable v_var : natural;
  begin
    proc(v_var);
    return v_var;
  end function;

  pure function test2 return natural is
    variable v_var : natural;
  begin
    proc2(v_var);
    return v_var;
  end function;

  pure function test3 return natural is
    variable v_var : natural;
  begin
    proc3(v_var);
    return v_var;
  end function;
end package body;
