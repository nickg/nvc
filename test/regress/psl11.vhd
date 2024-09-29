entity psl11 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl11 is
    signal clk : std_logic := '0';
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

    one: assert always a -> (eventually! b);
    two: assert always a -> (eventually! {not a; not a; b});

    stim: process is
    begin
        pulse(clk);
        a <= '1';
        pulse(clk);
        b <= '1';
        a <= '0';
        pulse(clk);
        a <= '1';
        pulse(clk);

        std.env.finish;                 -- Assertion fails here
    end process;

end architecture;
