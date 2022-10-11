entity issue551 is
end entity;

use std.textio.all;

architecture test of issue551 is
begin

    p1: process is
        file f : text;
        variable l : line;
    begin
        file_open(f, "out.txt", WRITE_MODE);
        write(l, string'("hello"));
        writeline(f, l);
        file_close(f);

        file_open(f, "out.txt", APPEND_MODE);
        write(l, string'("world"));
        writeline(f, l);
        file_close(f);

        file_open(f, "out.txt", READ_MODE);
        readline(f, l);
        assert l.all = "hello";
        readline(f, l);
        assert l.all = "world";
        assert endfile(f);
        file_close(f);

        wait;
    end process;

end architecture;
