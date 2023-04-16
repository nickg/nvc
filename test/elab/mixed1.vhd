entity mixed1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed1 is
    signal x, y, z : std_logic;
begin

    b1: block is
        component mod1 is
            port ( one, two : in std_logic;
                   three : out std_logic );
        end component;
    begin
        u: component mod1
            port map ( x, y, z );       -- OK
    end block;

    b2: block is
        component mod1 is
            port ( one : in std_logic;
                   three : out std_logic );
        end component;
    begin
        u: component mod1
            port map ( x, z );       -- Error
    end block;

    b3: block is
        component mod1 is
            port ( one, two : in std_logic;
                   three, four : out std_logic );
        end component;
    begin
        u: component mod1
            port map ( x, y, z, z );       -- Error
    end block;

    b4: block is
        component mod1 is
            port ( one, two : in std_logic;
                   three : out bit );
        end component;
    begin
        u: component mod1
            port map ( x, y, open );       -- OK
    end block;

end architecture;
