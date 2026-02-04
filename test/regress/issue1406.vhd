entity issue1406 is end entity;

architecture test of issue1406 is
  signal bools : BOOLEAN_VECTOR(0 to 1) := (others => false);
begin

  main : process begin
    wait for 2 ns;
    bools(0) <= true;
    wait for 2 ns;
    bools(1) <= true;
    wait for 2 ns;
    bools(0) <= false;
    wait for 2 ns;
    wait;
  end process;

end architecture;
