entity sub is
    generic ( g : integer := 1 );
    port ( p : in bit );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity vhpi24 is
end entity;

architecture test of vhpi24 is
    component sub is
        generic ( g : integer := 1 );
        port ( p : in bit );
    end component;

    -- No entity exists for this component so the instance stays unbound
    component nothere is
        generic ( g : integer := 2 );
        port ( p : in bit );
    end component;

    signal x : bit;
begin

    i0: component sub generic map (g => 3) port map (p => x);
    i1: component nothere port map (p => x);
    i2: entity work.sub port map (p => x);

end architecture;
