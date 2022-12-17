package synopsys_perf is
    procedure test_unsigned_plus;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

package body synopsys_perf is

    procedure test_unsigned_plus is
        constant WIDTH : integer := 8;
        constant ITERS : integer := 1;
        constant ONE   : unsigned(WIDTH - 1 downto 0) := conv_unsigned(1, WIDTH);
        variable s     : unsigned(WIDTH - 1 downto 0) := (others => '0');
    begin
        for i in 1 to ITERS loop
            for j in 0 to integer'(2 ** WIDTH - 1) loop
                s := s + ONE;
            end loop;
        end loop;
    end procedure;

end package body;
