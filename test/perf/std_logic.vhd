package std_logic_perf is
    procedure test_to_x01;
end package;

library ieee;
use ieee.std_logic_1164.all;

package body std_logic_perf is

    procedure test_to_x01 is
        constant ITERS : integer := 100;
        variable s     : std_logic_vector(31 downto 0);
    begin
        for i in 1 to ITERS loop
            s := "Z10-H01U10101L00Z10-H01U10101L00";
            s := to_x01(s);
            assert s = "X10X101X10101000X10X101X10101000";
        end loop;
    end procedure;

end package body;
