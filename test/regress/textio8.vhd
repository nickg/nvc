--
-- LINE indexes may not start from 1
--
entity textio8 is
end entity;

use std.textio.all;

architecture test of textio8 is
begin

    p1: process is
        variable l : line;
        variable i : integer;
        variable s : string(5 to 9);
        variable c : character;
        variable b : boolean;
        variable r : real;
        variable t : time;
        variable v : bit_vector(8 downto 1);
    begin
        l := new string(5 to 10);
        l.all := "123 45";
        read(l, i);
        assert i = 123;
        read(l, i);
        assert i = 45;

        l := new string(13 downto 3);
        l.all := "hello world";
        read(l, s);
        assert s = "hello";
        read(l, s);
        assert s = " worl";
        read(l, c);
        assert c = 'd';

        l := new string(5 to 8);
        l.all := "true";
        read(l, b);
        assert b = true;

        l := new string(5 downto 5);
        l.all := "x";
        read(l, c);
        assert c = 'x';

        l := new string(5 to 9);
        l.all := "2.000";
        read(l, r);
        assert r = 2.0;

        l := new string(5 to 9);
        l.all := "2.0e2";
        read(l, r);
        assert abs(r - 200.0) < 0.0001;

        l := new string(10 downto 7);
        l.all := "5 ns";
        read(l, t);
        assert t = 5 ns;

        l := null;
        s := "hello";
        write(l, s);
        assert l.all = "hello";
        assert l'left = 1;
        assert s'left = 5;

        l := new string(80 downto 73);
        l.all := X"ab";
        read(l, v);
        assert v = X"ab";

        l := new string(4 to 9);
        l.all := "-2.000";
        read(l, r);
        assert r = -2.0;

        wait;
    end process;

end architecture;
