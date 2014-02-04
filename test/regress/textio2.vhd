entity textio2 is
end entity;

use std.textio.all;

architecture test of textio2 is
    file tmp : text;
begin

    process is
        variable l    : line;
        variable str  : string(1 to 5);
        variable good : boolean;
    begin
        file_open(tmp, "tmp.txt", WRITE_MODE);
        write(l, string'("hello, world"));
        writeline(tmp, l);
        file_close(tmp);

        file_open(tmp, "tmp.txt", READ_MODE);

        readline(tmp, l);
        read(l, str);
        assert str = "hello";
        read(l, str);
        assert str = ", wor";
        read(l, str, good);
        assert not good;                -- Fewer than 5 chars

        file_close(tmp);
        wait;
    end process;

end architecture;
