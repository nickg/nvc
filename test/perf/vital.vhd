package vital is
    procedure test_statetable;
end package;

library ieee;
use ieee.vital_timing.all;
use ieee.vital_primitives.all;
use ieee.std_logic_1164.all;

package body vital is
    constant DFFTable: VitalStateTableType :=
        -- RESET  D   CLK  State Q
        (( '0',  '-', '-', '-', '0'),
         ( '1',  '1', '/', '-', '1'),
         ( '1',  '0', '/', '-', '0'),
         ( '1',  'X', '/', '-', 'X'),
         ( '1',  '-', '-', '-', 'S'));

    procedure test_statetable is
        variable d, q   : std_logic := '0';
        variable clk    : std_logic := '0';
        variable reset  : std_logic := '0';
        variable prev   : std_logic_vector(1 to 3);
        variable result : std_logic;
    begin
        vitalstatetable(result, prev, dfftable, (reset, d, clk));
        clk := '1';
        vitalstatetable(result, prev, dfftable, (reset, d, clk));
        clk := '0';
        for i in 1 to 1000 loop
            clk := '1';
            vitalstatetable(result, prev, dfftable, (reset, d, clk));
            clk := '0';
            vitalstatetable(result, prev, dfftable, (reset, d, clk));
            d := not q;
        end loop;
        assert q = '0';
    end procedure;

end package body;
