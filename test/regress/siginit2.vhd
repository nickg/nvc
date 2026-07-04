library ieee;
use ieee.std_logic_1164.all;

entity siginit2 is
end entity;

architecture test of siginit2 is

    type t_state is (
        IDLE,
        ACTIVE,
        DONE
    );

    signal sl  : std_logic;
    signal slv : std_logic_vector(3 downto 0);
    signal cnt : integer range 0 to 15;
    signal st  : t_state;

begin

    process is
    begin
        wait for 0 ns;

        assert sl = '1';
        assert slv = "1111";
        assert cnt = 15;
        assert st = DONE;

        wait;
    end process;

end architecture;
