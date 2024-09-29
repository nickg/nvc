entity psl9 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl9 is
    signal clk : std_logic := '0';
    signal a, b, c : std_logic := '0';

    default clock is rising_edge(clk);

    procedure pulse (signal clk : out std_logic) is
    begin
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
    end procedure;
begin

    hdl_impl: assert always (a -> b or c);

    stim: process is
    begin
        pulse(clk);
        b <= '1';
        pulse(clk);
        a <= '1';
        pulse(clk);
        c <= '1';
        pulse(clk);
        b <= '0';
        pulse(clk);
        c <= '0';
        pulse(clk);                     -- Assertion fails here

        wait;
    end process;

end architecture;
