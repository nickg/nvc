package numeric_std_perf is
    procedure test_to_unsigned;
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

end package body;
