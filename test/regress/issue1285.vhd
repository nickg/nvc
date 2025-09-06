library ieee;
library std;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue1285 is
end issue1285;

architecture tb of issue1285 is

  component test_core
    generic (
      DATAIN_WIDTH : integer;
      N            : integer;
      BITGROWTH    : integer
    );
    port (
      clk_i     : in  std_logic;
      rst_i     : in  std_logic;
      din_i     : in  std_logic_vector(DATAIN_WIDTH - 1 downto 0);
      dout_o    : out std_logic_vector(DATAIN_WIDTH + BITGROWTH - 1 downto 0)
    );
  end component;

  --------- Procedure Declaration ---------
  -- Clock Generation
  procedure f_gen_clk(constant freq : in    natural;
                      signal   clk  : inout std_logic) is
  begin
    loop
      wait for (0.5 / real(freq)) * 1 sec;
      clk <= not clk;
    end loop;
  end procedure f_gen_clk;

  constant  c_clk_freq  : natural   := 100e3; -- Clk Freq
  signal clk, rst       : std_logic := '0';
  signal dout : std_logic_vector(31 downto 0);
  signal ctr : unsigned(15 downto 0) := X"0000";

begin
  ------ Clock generation ------
  f_gen_clk(100e6, clk);

  ctr <= ctr + 1 when rising_edge(clk);

  p_wait: process
  begin
    report "rst <= 0";
    rst <= '0';

    wait for 100 ns;

    report "rst <= 1";
    rst <= '1';

    wait for 100 ns;
    rst <= '0';

    wait for 1 us;

    report to_hstring(dout);

    assert dout = X"0003FA5C";

    report "Finished";
    std.env.finish;
  end process p_wait;

  --------- Entity declaration ---------
  UUT: test_core
    generic map (
      DATAIN_WIDTH  => 16,
      N             => 2,
      BITGROWTH     => 16
      )
    port map (
      clk_i => clk,
      rst_i => rst,
      din_i => std_logic_vector(ctr),
      dout_o => dout );
end tb;
