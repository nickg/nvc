entity tcl4 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of tcl4 is
    type t_dir is (UP, DOWN);

    signal clk : std_logic := '0';
    signal ctr : unsigned(3 downto 0) := X"0";
    signal dir : t_dir := UP;
    signal int : integer := 1;
begin

    p1: clk <= not clk after 5 ns;

    p2: process (clk) is
    begin
        if rising_edge(clk) then
            case dir is
                when UP =>
                    ctr <= ctr + 1;
                when DOWN =>
                    ctr <= ctr - 1;
            end case;
        end if;
    end process;

end architecture;
