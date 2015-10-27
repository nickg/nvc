entity real3 is
end real3;

architecture Behavioral of real3 is

  signal value_real : real := -523467.0;
  signal value_integer : integer;

begin

  process
  begin
    report "real: " & real'image(value_real);
    value_integer <= integer(value_real);
    wait for 0 ns;
    report "integer: " & integer'image(value_integer);
    wait;
  end process;

end Behavioral;
