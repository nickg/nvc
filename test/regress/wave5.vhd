entity wave5 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of wave5 is
    type pair is record
        a, b : integer;
    end record;

    type rec is record
        x : integer;
        y : std_logic_vector(1 to 3);
        z : pair;
    end record;

    signal r : rec;

begin

    main: process is
    begin
        wait for 1 ns;
        r.y <= "101";
        wait for 1 ns;
        r.z.b <= 5;
        r.z.a <= 6;
        wait for 1 ns;
        r.x <= 2;
        r.z.a <= 1;
        wait;
    end process;

end architecture;
