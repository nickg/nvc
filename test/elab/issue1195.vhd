entity sub is
    generic ( name : integer );
    port ( BAZ : bit );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity issue1195 is
end entity;

architecture test of issue1195 is
    component sub is
        generic ( NAME : integer );
        port ( Baz : bit );
    end component;
begin

    u: component sub
        generic map ( NaMe => 6 )       -- OK
        port map ( baZ => '1' );        -- OK

end architecture;
