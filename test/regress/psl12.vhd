entity psl12 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl12 is
    signal clk, reset, areset : std_logic := '0';
    signal a, b : std_logic := '0';

    default clock is rising_edge(clk);

    procedure pulse (signal clk : out std_logic) is
    begin
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
    end procedure;
begin

    one: assert always (a -> next [2] (b)) sync_abort reset;
    two: assert always (a -> next [2] (b)) async_abort areset;

    stim: process is
    begin
        a <= '1';
        pulse(clk);
        a <= '0';
        pulse(clk);
        reset <= '1';

        -- Not asserted over clock edge
        -- TODO: bug when areset and clk change in same delta cycle
        wait for 0 fs;
        areset <= '1';
        wait for 1 fs;
        areset <= '0';

        pulse(clk);
        pulse(clk);

        wait;
    end process;

end architecture;
