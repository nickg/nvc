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
        deallocate(l);
        wait;
    end process;

    test_hread: process is
        variable l    : line;
        variable b    : bit_vector(1 to 16);
        variable bv4  : bit_vector(12 downto 0);
        variable bv7  : bit_vector(10 downto 0);
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
        deallocate(l);

        l := new string'("0ABC");
        hread (L, bv4, good);
        assert (good and bv4 = "0101010111100")
            report "h) short std.textio.hread " & to_string (bv4) severity error;
        deallocate (l);

        L := new string'("821");            -- one bit too many
        hread (L, bv7, good);
        assert (not good)
            report "l) std.textio.hread reported good read" severity error;
        deallocate(l);

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
        deallocate(l);
        wait;
    end process;

    test_ohwrite: process is
        variable l : line;
    begin
        hwrite(l, "1010", field => 5);
        assert l.all = "    A";
        deallocate(l);
        hwrite(l, X"abc", field => 5, justified => left);
        assert l.all = "ABC  ";
        deallocate(l);
        owrite(l, o"77");
        assert l.all = "77";
        deallocate(l);
        wait;
    end process;

end architecture;
