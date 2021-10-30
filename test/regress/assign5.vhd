entity assign5 is
end entity;

architecture test of assign5 is
    type rec1 is record
        x : bit;
        y : bit_vector(1 to 3);
        z : integer;
    end record;

    type rec2 is record
        x : rec1;
        y : character;
    end record;

    type bv2d is array (natural range <>) of bit_vector(1 to 2);
begin

    p1: process is
        variable a : bit;
        variable b : bit_vector(1 to 3);
        variable c : integer;
        variable d : character;
        variable p, q, r : bit;

        variable r1 : rec1;
        variable r2 : rec2;
    begin
        r1 := ('1', "010", 42);
        (a, b, c) := r1;
        wait for 1 ns;
        assert a = '1';
        assert b = "010";
        assert c = 42;

        r2 := (('0', "100", 72), 'Z');
        ((a, b, c), d) := r2;
        wait for 1 ns;
        assert a = '0';
        assert b = "100";
        assert c = 72;
        assert d = 'Z';

        (a, (p, q, r), c) := r1;
        wait for 1 ns;
        assert a = '1';
        assert p = '0';
        assert q = '1';
        assert r = '0';
        assert c = 42;

        (b(1 to 2), (q, r)) := bv2d'( "10", "01" );
        wait for 1 ns;
        assert b = "100";

        wait;
    end process;

end architecture;
