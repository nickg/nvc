entity issue1473 is
end entity;

library ieee;
use ieee.std_logic_1164.std_logic;

architecture test of issue1473 is
    component sub is
        port ( i : in std_logic;
               o : out std_logic );
    end component;

    signal i, o : std_logic;
begin

    uut: component sub port map (i, o);

end architecture;
