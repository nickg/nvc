
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover6 is
end entity;

architecture test of cover6 is

    signal from_u_to_1  : std_logic := 'U';
    signal from_u_to_0  : std_logic := 'U';

    signal from_z_to_1  : std_logic := 'Z';
    signal from_z_to_0  : std_logic := 'Z';

    signal from_1_to_z  : std_logic := '1';
    signal from_0_to_z  : std_logic := '0';

begin

    process
    begin
        wait for 1 ns;

        -- U -> 1
        from_u_to_1 <= '1';
        wait for 1 ns;

        -- U -> 0
        from_u_to_0 <= '0';
        wait for 1 ns;

        -- Z -> 1
        from_z_to_1 <= '1';
        wait for 1 ns;

        -- Z -> 0
        from_z_to_0 <= '0';
        wait for 1 ns;

        -- 1 -> Z
        from_1_to_z <= 'Z';
        wait for 1 ns;

        -- 0 -> Z
        from_0_to_z <= 'Z';
        wait for 1 ns;

        wait;
    end process;

end architecture;
