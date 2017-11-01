use     std.textio.all;
entity  ISSUE351 is
end     ISSUE351;
architecture RTL of ISSUE351 is
    type      WORD_TYPE    is record
              KEY          :  integer;
              VALUE        :  integer;
    end record;
    constant  WORD_NULL    :  WORD_TYPE := (KEY => 0, VALUE => 0);
    type      WORD_VECTOR  is array (INTEGER range <>) of WORD_TYPE;
    procedure dump_words(WORDS: in WORD_VECTOR)
    is
        variable  text_line      : LINE;
    begin
        for i in WORDS'range loop
            WRITE(text_line, string'(" | "));
            WRITE(text_line, i, LEFT, 5);
            WRITE(text_line, string'(" | "));
            WRITE(text_line, WORDS(i).KEY  , LEFT, 10);
            WRITE(text_line, string'(" | "));
            WRITE(text_line, WORDS(i).VALUE, LEFT, 10);
            WRITELINE(OUTPUT, text_line);
        end loop;
    end procedure;
begin
    process
        variable curr_queue : WORD_VECTOR(0 to 3);
        variable  text_line : LINE;
    begin
        for i in curr_queue'range loop
            curr_queue(i).KEY   := i;
            curr_queue(i).VALUE := i;
        end loop;
        for i in curr_queue'range loop
            WRITE(text_line, i);
            WRITE(text_line, string'(" to "));
            WRITE(text_line, curr_queue'length-1);
            WRITELINE(OUTPUT, text_line);
            dump_words(curr_queue(i to curr_queue'length-1)); -- Bug?
        end loop;
        wait;
    end process;
end RTL;
