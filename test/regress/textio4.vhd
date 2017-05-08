entity textio4 is
end entity;

use std.textio.all;

architecture test of textio4 is
begin

    check_int: process is
        variable l : line;
        variable x : integer;
        variable good : boolean;
        variable c : character;
    begin
        l := new string'("  123,5");
        read(l, x, good);
        assert good;
        assert x = 123;
        assert l.all(1) = ',';
        read(l, x, good);
        assert not good;
        assert l.all(1) = ',';
        read(l, c, good);
        assert good;
        assert c = ',';
        assert l.all(1) = '5';
        read(l, x, good);
        assert good;
        assert x = 5;
        assert l.all'length = 0;
        wait;
    end process;

    check_bool: process is
        variable l : line;
        variable x : boolean;
        variable good : boolean;
        variable c : character;
    begin
        l := new string'("  true,false");
        read(l, x, good);
        assert good;
        assert x = true;
        assert l.all(1) = ',';
        read(l, x, good);
        assert not good;
        assert l.all(1) = ',';
        read(l, c, good);
        assert good;
        assert c = ',';
        assert l.all(1) = 'f';
        read(l, x, good);
        assert good;
        assert x = false;
        assert l.all'length = 0;
        wait;
    end process;

    check_real: process is
        variable l : line;
        variable x : real;
        variable good : boolean;
        variable c : character;
    begin
        l := new string'("  5.152,61.4,5.");
        read(l, x, good);
        assert good;
        assert x > 5.151 and x < 5.153;
        assert l.all(1) = ',';
        read(l, x, good);
        assert not good;
        assert l.all(1) = ',';
        read(l, c, good);
        assert good;
        assert c = ',';
        assert l.all(1) = '6';
        read(l, x, good);
        assert good;
        assert x > 61.39 and x < 61.41;
        read(l, c, good);
        assert good;
        assert c = ',';
        assert l.all(1) = '5';
        read(l, x, good);
        assert not good;
        wait;
    end process;


end architecture;
