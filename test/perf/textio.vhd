package textio_perf is
    procedure test_write;
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
end package body;
