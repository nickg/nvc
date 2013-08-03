entity textio1 is
end entity;

use std.textio.all;

architecture test of textio1 is
begin

    process is
        variable l : line;
    begin
        write(l, string'("hello, world"));
        writeline(output, l);
        assert l'length = 0;

        write(l, string'("one"));
        write(l, ' ');
        write(l, string'("two"));
        writeline(output, l);

        write(l, string'("hello"), left, 10);
        write(l, '|');
        write(l, string'("world"), right, 10);
        writeline(output, l);

        write(l, bit'( '0' ), left, 4);
        write(l, bit_vector'("0110101"));
        writeline(output, l);

        wait;
    end process;

end architecture;
