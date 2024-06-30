entity issue906 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of issue906 is
    signal x, y : std_logic_vector(1 to 3);
begin

    b: block is
        port ( p : in std_logic_vector;
               q : out std_logic_vector );
        port map ( x, y );
    begin
        q <= x after 1 ns;
    end block;

    x <= "100" after 1 ns, "111" after 2 ns;

end architecture;
