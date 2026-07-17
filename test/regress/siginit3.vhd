library ieee;
use ieee.std_logic_1164.all;

entity siginit3 is
end entity;

architecture test of siginit3 is

    type t_state is (
        IDLE,
        ACTIVE,
        DONE
    );

    signal sl  : std_logic;
    signal cnt : integer range 0 to 15;
    signal st  : t_state;

begin

    process is
    begin
        wait for 0 ns;

        -- corresponds to initialization based on given seed
        -- reproducible with randomizer algorithm in signit.c
        assert sl = '1';
        assert cnt = 3;
        assert st = ACTIVE;

        wait;
    end process;

end architecture;
