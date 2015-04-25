use std.textio.all;
entity issue103 is
end entity;
architecture test of issue103 is

    procedure puts(TAG:inout LINE; MSG:in STRING) is
        variable text_line   : LINE;
    begin
        write(text_line, TAG(TAG'range), RIGHT,  10);
        write(text_line, string'(" "));
        write(text_line, MSG);
        writeline(OUTPUT, text_line);
    end procedure;
begin
    main: process
        variable tag  : line;
    begin
        write(tag , string'(">>"));
        puts(tag, "test");
        wait;
    end process;
end test;
