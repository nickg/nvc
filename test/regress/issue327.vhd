library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.log2;
use ieee.math_real.ceil;

package test_pkg is
  function min_bits (val : positive)
    return integer;
end test_pkg;

package body test_pkg is
  function min_bits (val : positive)
    return integer is
    variable rv : integer;
  begin
    rv := integer(ceil(log2(real(val))));
    if rv = 0 then
      rv := 1;
    end if;
    return rv;
  end;
end test_pkg;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
  generic (
    MAX_VAL : positive;
    WIDTH   : positive
  );
  port (
    clk   : in  std_logic;
    rst   : in  std_logic;
    val_o : out unsigned(WIDTH-1 downto 0)
  );
end entity counter;

architecture rtl of counter is
  signal val : unsigned(val_o'range);
begin  -- architecture rtl
  val_o <= val;
  process is
  begin  -- process
    wait until rising_edge(clk);
    val <= val + 1;
    if rst = '1' then
      val <= (others => '0');
    end if;
  end process;
end architecture rtl;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test_pkg.all;

entity issue327 is
end entity issue327;

architecture tb of issue327 is
  constant MAX_VAL : positive  := 5;
  constant WIDTH   : positive  := min_bits(MAX_VAL);
  signal clk       : std_logic := '1';
  signal rst       : std_logic := '1';
  signal val_o     : unsigned(WIDTH-1 downto 0);
  signal val_o_d   : unsigned(WIDTH-1 downto 0);
begin  -- architecture tb
  DUT : entity work.counter
    generic map (
      MAX_VAL => MAX_VAL,
      WIDTH   => WIDTH)
    port map (
      clk   => clk,
      rst   => rst,
      val_o => val_o);

  process is
  begin
      assert WIDTH = 3;
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      assert val_o = "000";
      rst <= '0';
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
      assert val_o = "001";
      wait;
  end process;
end architecture tb;
