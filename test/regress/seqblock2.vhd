-- https://gitlab.com/IEEE-P1076/VHDL-Issues/-/issues/281
entity seqblock2 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of seqblock2 is
    signal x : std_logic_vector(1 to 3) := "ZZZ";
begin

    process is
    begin
        for i in 1 to 3 loop
            block is
                constant c : natural := i;
            begin
                x(c) <= '1';
            end block;
        end loop;
        wait;
    end process;

    x(1) <= '0';

    process is
    begin
        wait for 1 ns;
        assert x = "X11";
        wait;
    end process;

end architecture;
