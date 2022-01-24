entity textio2 is
end entity;

use std.textio.all;

architecture test of textio2 is
begin

    process is
        file tmp      : text;
        variable l    : line;
        variable str  : string(1 to 5);
        variable good : boolean;
        variable ch   : character;
    begin
        file_open(tmp, "tmp.txt", WRITE_MODE);
        write(l, string'("hello, world"));
        writeline(tmp, l);
        write(l, string'("second"));
        writeline(tmp, l);
        deallocate(l);
        file_close(tmp);

        file_open(tmp, "tmp.txt", READ_MODE);

        readline(tmp, l);
        read(l, str);
        assert str = "hello";
        read(l, str);
        assert str = ", wor";
        read(l, str, good);
        assert not good;                -- Fewer than 5 chars
        deallocate(l);

        readline(tmp, l);
        read(l, str);
        assert str = "secon";
        read(l, ch);
        assert ch = 'd';
        read(l, ch, good);
        assert not good;
        deallocate(l);

        file_close(tmp);
        wait;
    end process;

end architecture;
