package pack is
    component comp is
        generic ( width1 : integer; width2 : integer := width1 + 1 );
        port ( x : in bit_vector(width2 - 1 downto 0) );
    end component;
end package;

-------------------------------------------------------------------------------

entity comp1 is
end entity;

use work.pack.all;

architecture test of comp1 is
    for all : comp use open;
begin

    u: component comp
        generic map ( width1 => 8 )
        -- The type of this aggregate references the generic WIDTH2
        -- prior to folding
        port map ( x => (others => '0') );

end architecture;
