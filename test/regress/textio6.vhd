entity textio6 is
end entity;

use std.textio.all;

architecture test of textio6 is
begin

    test_sread: process is
        variable l : line;
        variable s : string(1 to 5);
        variable n : natural;
    begin
        l := new string'("  word foo bar_baz  end");
        sread(l, s, n);
        report s(1 to n);
        assert n = 4;
        assert s(1 to n) = "word";
        sread(l, s, n);
        report s(1 to n);
        assert n = 3;
        assert s(1 to n) = "foo";
        sread(l, s, n);
        report s(1 to n);
        assert n = 5;
        assert s(1 to n) = "bar_b";
        sread(l, s, n);
        report s(1 to n);
        assert n = 2;
        assert s(1 to n) = "az";
        sread(l, s, n);
        report s(1 to n);
        assert n = 3;
        assert s(1 to n) = "end";
        sread(l, s, n);
        assert n = 0;
        wait;
    end process;

    test_hread: process is
        variable l    : line;
        variable b    : bit_vector(1 to 16);
        variable good : boolean;
    begin
        l := new string'("  1 2 ab23 12_34 dead 12 1__2");
        hread(l, b(1 to 4));
        assert b(1 to 4) = "0001";
        hread(l, b(1 to 4));
        assert b(1 to 4) = "0010";
        hread(l, b(1 to 16));
        assert b(1 to 16) = X"ab23";
        hread(l, b(1 to 16));
        assert b(1 to 16) = X"1234";
        hread(l, b(1 to 16));
        assert b(1 to 16) = X"dead";
        hread(l, b(1 to 16), good);
        assert not good;
        hread(l, b(1 to 8), good);
        assert not good;

        wait;
    end process;

    test_oread: process is
        variable l    : line;
        variable b    : bit_vector(1 to 6);
        variable good : boolean;
    begin
        l := new string'("  1 2 5_2");
        oread(l, b(1 to 3));
        assert b(1 to 3) = "001";
        oread(l, b(1 to 3));
        assert b(1 to 3) = "010";
        oread(l, b(1 to 6));
        report to_string(b(1 to 6));
        assert b(1 to 6) = "101010";

        wait;
    end process;

end architecture;
