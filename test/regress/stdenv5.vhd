entity stdenv5 is
end entity;

use std.env.all;

architecture test of stdenv5 is
begin

    p1: process is
        variable t1, t2, t3 : time_record;
    begin
        t1 := gmtime;
        report "GMTIME: " & to_string(t1);
        t2 := localtime(t1);
        report "LOCALTIME: " & to_string(t2);
        assert t1.microsecond = t2.microsecond;
        assert t1.second = t2.second;
        assert abs (t1.year - t2.year) <= 1;
        assert abs (t1.hour - t2.hour) <= 24;
        assert abs (t1.day - t2.day) <= 1;
        assert abs (t1.month - t2.month) <= 1;

        report "Recovered GMTIME: " & to_string(gmtime(t2));
        assert gmtime(t2) = t1;

        assert epoch(t1) > 0.0;
        assert abs(epoch(t2) - epoch(t1)) < real(24 * 3600.0);

        t3 := t1 + 1.0;
        report to_string(t1) & " + 1.0 ==> " & to_string(t3);
        assert t3.second = (t1.second + 1) mod 60;

        t3 := t1 + 0.999999999;
        report to_string(t1) & " + 0.99999999 ==> " & to_string(t3);
        assert t3.second = (t1.second + 1) mod 60;

        t3 := t1 - 3.0;
        report to_string(t1) & " - 3.0 ==> " & to_string(t3);
        assert t3.second = (t1.second - 3) mod 60;

        t3 := t1 - 0.999999999;
        report to_string(t1) & " - 0.99999999 ==> " & to_string(t3);
        assert t3.second = (t1.second - 1) mod 60;

        wait;
    end process;

end architecture;
