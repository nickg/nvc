entity issue899 is
end entity;

architecture beh of issue899 is
    constant C_TIME_1 : time := 8.2 ms;
    constant C_TIME_2 : time := 8200 us;
begin

  process
  begin
    assert C_TIME_1 = C_TIME_2 report "Not same " & to_string(C_TIME_1) & " /= " &  to_string(C_TIME_2) severity error;
    wait;
  end process;

end architecture beh;
