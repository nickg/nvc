entity psl18 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl18 is
    signal clk : std_logic := '0';
    signal i : integer := 0;
    signal x : bit_vector(1 to 3) := "101";

    default clock is rising_edge(clk);

    procedure pulse (signal clk : out std_logic) is
    begin
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
    end procedure;
begin

    one: assert always i = prev(i) + 1;  -- Fails at 7ns
    two: assert always x = not prev(x);  -- Fails at 5ns

    stim: process is
    begin
        i <= i + 1;
        x <= not x;
        pulse(clk);
        x <= not x;
        i <= i + 1;
        pulse(clk);
        i <= i + 1;
        pulse(clk);                     -- Error
        x <= not x;
        pulse(clk);                     -- Error

        wait;
    end process;

end architecture;
