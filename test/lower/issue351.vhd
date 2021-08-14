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
    begin
    end procedure;
begin
    p1: process
        variable curr_queue : WORD_VECTOR(0 to 3);
    begin
        loop1: for i in curr_queue'range loop
            dump_words(curr_queue(i to curr_queue'length-1)); -- Bug?
        end loop;
        wait;
    end process;
end RTL;
