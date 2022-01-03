--
-- Test READ for real types
--
entity textio7 is
end entity;

use std.textio.all;

architecture test of textio7 is

    procedure check(value, expect : real) is
        variable l : line;
    begin
        assert abs(value - expect) < 0.0001
            report "value=" & real'image(value) & " expect=" & real'image(expect)
            severity failure;
        write(l, value);
        writeline(output, l);
    end procedure;

begin

    main: process is
        variable r : real;
        variable l : line;
    begin
        l := new string'("1.23");
        read(l, r);
        check(r, 1.23);

        l := new string'("+4");
        read(l, r);
        check(r, 4.0);

        l := new string'("-0.001");
        read(l, r);
        check(r, -0.001);

        l := new string'("1.23e2");
        read(l, r);
        check(r, 123.0);

        l := new string'("1.994500e+03");
        read(l, r);
        check(r, 1994.5);

        l := new string'("    1.994500e+03");
        read(l, r);
        check(r, 1994.5);

        wait;
    end process;

end architecture;
