package pack is
    type nat_vector is array (natural range <>) of natural;
end package;

-------------------------------------------------------------------------------

entity sub1 is
    port ( x : out natural );
end entity;

architecture test of sub1 is
begin

    p1: process is
    begin
        x <= 1;
        wait for 0 ns;
        assert x'event;
        assert x'active;
        wait for 0 ns;
        x <= 5;
        wait for 0 ns;
        assert x'event;
        assert x'active;
        x <= 5;
        wait for 0 ns;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub2 is
    port ( z : in nat_vector(1 to 3) );
end entity;

architecture test of sub2 is
begin

    p2: process is
    begin
        assert z = (0, 2, 3);
        wait for 0 ns;
        assert z = (1, 2, 3);
        assert z'active;
        assert z'event;
        assert z(1)'event;
        wait for 0 ns ;
        assert not z'active;
        assert not z'event;
        wait for 0 ns;
        assert z = (5, 2, 3);
        assert z'active;
        assert z'event;
        wait for 0 ns ;
        assert z'active;
        assert not z'event;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity signal21 is
end entity;

architecture test of signal21 is
    signal y : natural;
begin

    u1: entity work.sub1
        port map ( y );

    u2: entity work.sub2
        port map (
            z(1) => y,
            z(2) => 2,
            z(3) => 3 );

end architecture;
