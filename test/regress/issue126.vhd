library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned."-";
--use ieee.std_logic_arith.all;   -- Uncommenting fixed issue

entity issue126 is
end issue126;

architecture structural of issue126 is

    signal clk_count : std_logic_vector (2 downto 0);
begin

  process
  begin
        clk_count <= "110";
        wait for 1 us;
        clk_count <= clk_count - 1;
        wait for 1 us;
        assert clk_count = "101" severity error;

        wait;
    end process;
  end architecture structural;
