entity issue842 is
end entity;

architecture beh of issue842 is
  constant C_SCOPE : string := "something";
begin

  p_proc : process
    constant C_TIME : time := 1 ms;
  begin
    report string'("Test " & to_string(C_TIME, ms), C_SCOPE);
    assert string'("Test " & to_string(C_TIME, ms), C_SCOPE) = "Test 1 mssomething";
    wait;
  end process;
end architecture;
