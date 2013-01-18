entity proc5 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of proc5 is

    procedure next_cycle(signal clk : in std_logic; n : in integer := 1) is
    begin
        for i in 1 to n loop
            wait until rising_edge(clk);
            wait for 1 ns;
        end loop;
    end procedure;

    procedure create_clock
        ( signal clk     : inout std_logic;
          period         : in delay_length;
          signal running : in boolean ) is
    begin
        if running then
            clk <= not clk after period / 2;
        end if;
    end procedure;

    signal running : boolean := true;

    signal clk : std_logic := '1';

begin

    process (clk, running) is
    begin
        create_clock(clk, 20 ns, running);
    end process;

    process is
    begin
        next_cycle(clk, 50);
        assert now = 1001 ns;
        running <= false;
        wait;
    end process;

end architecture;
