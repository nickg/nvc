package directmap2_pack is
    type rec is record
        x : integer;
        y : bit;
        z : bit_vector(1 to 3);
    end record;
end package;

-------------------------------------------------------------------------------

use work.directmap2_pack.all;

entity bot1 is
    port ( r : in rec );
end entity;

architecture test of bot1 is
begin

    p1: process is
    begin
        wait for 1 ns;
        assert r = ( 5, '1', "101" );
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.directmap2_pack.all;

entity bot2 is
    port ( r : in rec );
end entity;

architecture test of bot2 is
begin

    p1: process is
    begin
        wait for 1 ns;
        assert r = ( 6, '1', "101" );
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.directmap2_pack.all;

entity record24 is
end entity;

architecture test of record24 is
    signal p : bit;
    signal q : bit_vector(1 to 3);
    signal i : integer;
begin

    uut1: entity work.bot1
        port map (
            r.x => 5,
            r.y => p,
            r.z => q );

    uut2: entity work.bot2
        port map (
            r.x => i,
            r.y => p,
            r.z => q );

    main: process is
    begin
        i <= 6;
        p <= '1';
        q <= "101";
        wait;
    end process;

end architecture;
