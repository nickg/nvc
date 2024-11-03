library ieee;
use ieee.std_logic_1164.all;

entity issue1044 is end entity;
architecture arch of issue1044 is
  component clk_gate_p is
  port (
    CLK : in std_logic;
    E : in std_logic;
    GCLK : out std_logic;
    SE : in std_logic);
  end component;
  component clk_gate_n is
  port (
    CLK : in std_logic;
    E : in std_logic;
    GCLK : out std_logic;
    SE : in std_logic);
  end component;
  signal CLK, SE, E, GCLK_p, GCLK_n : std_logic;
begin
  u0 : clk_gate_p
  port map (
    CLK => clk,
    E => e,
    GCLK => gclk_p,
    SE => se);
  u1 : clk_gate_n
  port map (
    CLK => clk,
    E => e,
    GCLK => gclk_n,
    SE => se);

  se <= '0';

  process is
  begin
      CLK <= '0';
      E <= '0';
      wait for 1 ps;
      CLK <= '1';
      wait for 1 ps;
      assert gclk_p = '0' severity failure;
      assert gclk_n = '1' severity failure;
      CLK <= '0';
      wait for 1 ps;
      assert gclk_p = '0' severity failure;
      assert gclk_n = '1' severity failure;
      E <= '1';
      wait for 1 ps;
      assert gclk_p = '0' severity failure;
      assert gclk_n = '1' severity failure;
      CLK <= '1';
      wait for 1 ps;
      assert gclk_p = '1' severity failure;
      assert gclk_n = '1' severity failure;
      CLK <= '0';
      wait for 1 ps;
      assert gclk_p = '0' severity failure;
      assert gclk_n = '0' severity failure;
    wait;
  end process;
end architecture;
