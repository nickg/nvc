entity mixed2 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed2 is
begin

    b1: block is
        component mod1 is
            port ( i1 : in std_logic_vector;
                   o1 : out std_logic_vector(3 downto 0) );
        end component;
    begin
        u: component mod1
            port map ( X"fffff", open );       -- Error
    end block;

    b2: block is
        component mod1 is
            port ( i1 : in std_logic_vector(5 downto 0);
                   o1 : out std_logic_vector(3 downto 0) );
        end component;
    begin
        u: component mod1
            port map ( (others => '1'), open );       -- Error
    end block;

    b3: block is
        component mod1 is
            port ( i1 : in std_logic_vector(7 downto 0);
                   o1 : out std_logic_vector );
        end component;
        signal s : std_logic_vector(99 downto 0);
    begin
        u: component mod1
            port map ( X"00", s );       -- Error
    end block;

    b4: block is
        component mod1 is
            port ( i1 : in std_logic_vector(7 downto 0);
                   o1 : out std_logic_vector(7 downto 0) );
        end component;
        signal s : std_logic_vector(7 downto 0);
    begin
        u: component mod1
            port map ( X"00", s );       -- Error
    end block;

end architecture;
