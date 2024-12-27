entity driver23 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of driver23 is
    type t_rec is record
        x : std_logic;
        y : std_logic_vector(7 downto 0);
    end record;

    signal s : t_rec;
begin

    s <= ('1', X"Z0");

    b: block is
        port ( p : out t_rec );
        port map ( s );
    begin
        check: process is
        begin
            p <= ('0', X"5Z");
            wait for 0 ns;
            report to_string(p.x);
            report to_string(p.y);
            assert p = ('0', X"5Z");
            assert p'driving_value = ('0', X"5Z");
            wait;
        end process;
    end block;

end architecture;
