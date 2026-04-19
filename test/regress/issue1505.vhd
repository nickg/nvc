library ieee;
use ieee.std_logic_1164.all;

entity subentity is
    port (
        a : in std_logic_vector(0 downto 0);
        b : out std_logic_vector(0 downto 0)
    );
end entity;

architecture test of subentity is
begin
    b <= a;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity issue1505 is
end entity;

architecture test of issue1505 is
    signal ain    : std_logic_vector(1 downto 0);
    signal b1, b2 : std_logic_vector(0 downto 0);
    signal aslice : std_logic_vector(0 downto 0);
begin
    aslice <= ain(0 downto 0);

    entity_1: entity work.subentity
        port map (
            a => aslice,
            b => b1
        );

    entity_2: entity work.subentity
        port map (
            a => ain(0 downto 0),
            b => b2
        );
end architecture;
