entity issue824 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue824 is
    type t_rec is record
        x, y : std_logic;
    end record;

    procedure update (signal r : inout t_rec) is
    begin
        r.y <= '1';
    end procedure;

    signal s : t_rec := ('Z', 'Z');
begin

    p1: process is
    begin
        --s.x <= 'Z';
        wait for 1 ns;
        update(s);
        wait for 1 ns;
        assert s.x = 'Z';
        assert s.y = '1';
        wait;
    end process;

    b1: block is
        port ( o : out t_rec );
        port map ( s );
    begin
        o <= ('Z', 'Z');
    end block;

end architecture;
