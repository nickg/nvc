library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
  port (
    clk     : in std_logic;
    dataIn  : in std_logic_vector(15 downto 0);
    sel     : in unsigned(1 downto 0);
    en      : in std_logic
  );
end test;

architecture rtl of test is
  type v_slv_t is array (natural range<>) of std_logic_vector;

  signal test : v_slv_t(2 downto 0)(15 downto 0) := (others => (others => '0'));
begin
test_reg : process (clk)
begin
  if rising_edge(clk) then
    if en then
      test(to_integer(sel)) <= dataIn;
    end if;
  end if;
end process;
end rtl;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue957 is
end issue957;

architecture rtl of issue957 is
  signal clk_period : time := 20 ns;
  signal clk : std_logic := '0';
  signal dataIn : std_logic_vector(15 downto 0) := (others => '0');
  signal sel : unsigned(1 downto 0) := (others => '0');
  signal en : std_logic := '0';
begin
  clk <= not clk after clk_period / 2;
  uut : entity work.test
    port map(
      clk => clk,
      dataIn => dataIn,
      sel => sel,
      en => en
    );

process
begin
  wait for 100 ns;
  wait until rising_edge(clk);
  dataIn <= std_logic_vector(to_unsigned(16#1111#, dataIn'length));
  sel <= to_unsigned(16#0#, sel'length);
  en <= '1';
  wait until rising_edge(clk);
  en <= '0';

  wait for 100 ns;
  wait until rising_edge(clk);
  dataIn <= std_logic_vector(to_unsigned(16#3333#, dataIn'length));
  sel <= to_unsigned(16#2#, sel'length);
  en <= '1';
  wait until rising_edge(clk);
  en <= '0';

  wait for 100 ns;

  wait until rising_edge(clk);
  dataIn <= std_logic_vector(to_unsigned(16#2222#, dataIn'length));
  sel <= to_unsigned(16#1#, sel'length);
  en <= '1';
  wait until rising_edge(clk);
  en <= '0';

  wait for 100 ns;

  std.env.stop;

end process;

end rtl;
