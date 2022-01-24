entity textio3 is
end entity;

use std.textio.all;

architecture test of textio3 is

    procedure check_content(expect : in string) is
        file tmp      : text is "tmp.txt";
        variable l    : line;
        variable str  : string(1 to expect'length);
        variable good : boolean;
        variable ch   : character;
    begin
        readline(tmp, l);
        read(l, str, good);
        assert good;
        assert str = expect;
        deallocate(l);
    end procedure;

begin

    process is
        file tmp   : text;
        variable l : line;
    begin
        file_open(tmp, "tmp.txt", WRITE_MODE);
        write(l, string'("hello, world"));
        writeline(tmp, l);
        file_close(tmp);

        check_content("hello, world");

        -- Check corner case of reading LINE_BUFFER_SIZE characters (issue #393)
        file_open(tmp, "tmp.txt", WRITE_MODE);
        write(l, string'("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"));
        writeline(tmp, l);
        file_close(tmp);

        check_content("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");

        deallocate(l);
        wait;
    end process;

end architecture;
