package pack is

    type r1 is record
        x : integer;
        y : character;
    end record;

    type r1_vec is array (natural range <>) of r1;

    type r2 is record
        p : r1;
        q : r1_vec(1 to 2);
    end record;

    type r2_vec is array (natural range <>) of r2;

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        x : in r2;
        y : out r2_vec(1 to 3) );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        wait for 1 ns;
        assert x = ( ( 1, '2' ), ( ( 3, '4' ), ( 5, '6' ) ) );
        y(3) <= ( ( 7, 'a' ), ( ( 8, 'b' ), ( 9, 'c' ) ) );
        wait on x;
        assert x = ( ( 1, '2' ), ( ( 42, '4' ), ( 5, '6' ) ) );
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity record22 is
end entity;

use work.pack.all;

architecture test of record22 is
    signal s : r2;
    signal t : r2_vec(1 to 3);
begin

    uut: entity work.sub port map ( s, t );

    main: process is
    begin
        s <= ( ( 1, '2' ), ( ( 3, '4' ), ( 5, '6' ) ) );
        wait for 2 ns;
        assert t(3) = ( ( 7, 'a' ), ( ( 8, 'b' ), ( 9, 'c' ) ) );
        s.q(1).x <= 42;
        wait;
    end process;

end architecture;
