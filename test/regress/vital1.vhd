library ieee;
use ieee.vital_timing.all;
use ieee.vital_primitives.all;
use ieee.std_logic_1164.all;

entity vital1 is
end entity;

architecture test of vital1 is
    constant DFFTable: VitalStateTableType :=
        -- RESET  D   CLK  State Q
        (( '0',  '-', '-', '-', '0'),
         ( '1',  '1', '/', '-', '1'),
         ( '1',  '0', '/', '-', '0'),
         ( '1',  'X', '/', '-', 'X'),
         ( '1',  '-', '-', '-', 'S'));

    signal d, q    : std_logic;
    signal clk     : std_logic := '0';
    signal reset   : std_logic := '0';
    signal running : boolean   := true;
begin

    p1: process (reset, d, clk) is
        variable prev   : std_logic_vector(1 to 3);
        variable result : std_logic;
    begin
        vitalstatetable(result, prev, dfftable, (reset, d, clk));
        q <= result;
    end process;

    clkp: clk <= not clk after 5 ns when running;

    p2: process is
    begin
        wait until falling_edge(clk);
        assert q = '0';

        d <= '1';
        reset <= '1';
        wait until falling_edge(clk);
        assert q = '1';

        d <= '0';
        wait until falling_edge(clk);
        assert q = '0';

        running <= false;
        wait;
    end process;

end architecture;
