entity tounsigned is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of tounsigned is

    constant WIDTH : integer := 20;
    constant ITERS : integer := 100;

    signal s : unsigned(WIDTH - 1 downto 0);
begin

    process is
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s <= to_unsigned(j, WIDTH);
            end loop;
        end loop;
        wait;
    end process;

end architecture;
