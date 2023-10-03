package textio_perf is
    procedure test_write;
    procedure test_read;
end package;

use std.textio.all;

package body textio_perf is
    procedure test_write is
        constant ITERS : integer := 10;
        variable l : line;
    begin
        for i in 1 to ITERS loop
            l := null;
            write(l, string'("hello, world"));
            write(l, true);
            write(l, bit'( '1' ));
            write(l, 5 ns, field => 10);
        end loop;
    end procedure;

    procedure test_read is
        constant ITERS : integer := 10;
        variable l : line;
        variable n : integer;
        variable g : boolean;
    begin
        for i in 1 to ITERS loop
            l := new string'("1234 5 145 14 123 52 1 2 14 1 512 12 5 14 1 5 10 5 1414 12341 -1 55 11 55");
            for j in 1 to 24 loop
                read(l, n);
            end loop;
            read(l, n, g);
            assert not g;
        end loop;
    end procedure;
end package body;
