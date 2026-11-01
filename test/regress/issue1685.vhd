package issue1685_pack is
    generic ( VALUE : integer );
end package;

-------------------------------------------------------------------------------

entity issue1685_sub is
    generic ( package PARAMS is new work.issue1685_pack generic map (<>) );
end entity;

architecture test of issue1685_sub is
begin
    p1: process is
    begin
        assert PARAMS.VALUE = 123;
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

entity issue1685_mod is
    generic ( package PARAMS is new work.issue1685_pack generic map (<>) );
end entity;

architecture test of issue1685_mod is
begin
    u: entity work.issue1685_sub
        generic map ( PARAMS => PARAMS );
end architecture;

-------------------------------------------------------------------------------

package issue1685_inst is new work.issue1685_pack generic map ( VALUE => 123 );

-------------------------------------------------------------------------------

entity issue1685 is
end entity;

architecture test of issue1685 is
begin
    u: entity work.issue1685_mod
        generic map ( PARAMS => work.issue1685_inst );
end architecture;
