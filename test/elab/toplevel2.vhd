entity toplevel2 is
    generic (
        I : integer;
        S : string;
        B : bit;
        V : bit_vector );
end entity;

architecture test of toplevel2 is
    signal x, y : integer;
    signal z : bit;
begin

    g1: if I = 4 generate
        x <= 2;
    end generate;

    g2: if I > 10 generate
        x <= 6;
    end generate;

    g3: if S = "hello" generate
        y <= 1;
    end generate;

    g4: if V = "101" generate
        z <= B;
    end generate;

end architecture;
