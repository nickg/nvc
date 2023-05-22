package numeric_std_perf is
    procedure test_to_unsigned;
    procedure test_add_unsigned;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package body numeric_std_perf is

    procedure test_to_unsigned is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        variable s     : unsigned(WIDTH - 1 downto 0);
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := to_unsigned(j, WIDTH);
            end loop;
        end loop;
    end procedure;

    procedure test_add_unsigned is
        constant WIDTH : integer := 16;
        constant ITERS : integer := 500;
        variable accum : unsigned(WIDTH - 1 downto 0) := (others => '0');
        constant one   : unsigned(WIDTH - 1 downto 0) := to_unsigned(1, WIDTH);
    begin
        for i in 1 to ITERS loop
            accum := accum + one;
        end loop;
        assert accum = to_unsigned(ITERS, WIDTH);
    end procedure;

end package body;
