entity issue1362 is end entity;

architecture enum_test of issue1362 is

  type state_t is (ONE, TWO, THREE);
  type state_array_t is array (0 to 1) of state_t;
  signal states : state_array_t := (others => ONE);

begin

  main : process begin
    wait for 2 ns;
    states(0) <= TWO;
    wait for 2 ns;
    states(1) <= TWO;
    wait for 2 ns;
    states(0) <= THREE;
    wait for 2 ns;
    wait;
  end process;

end architecture;
