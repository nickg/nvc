entity issue187 is
end entity;

architecture a of issue187 is
  function expensive_function_returning_false return boolean is
  begin
    report "This cost much";
    return false;
  end function;
begin

  main : process
  begin
    assert (not (expensive_function_returning_false or expensive_function_returning_false));
    wait;
  end process;
end architecture;
