entity top is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of top is
    component sub is
        generic ( i : integer );
        port ( o : out std_logic_vector(7 downto 0) );
    end component;
begin

    g: for i in 1 to 10 generate
        u: component sub
            generic map ( 42 )
            port map ( open );
    end generate;

end architecture;
