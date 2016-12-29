-- test_ng_1.vhd
use     std.textio.all;
entity  TEST_NG_1 is
    generic (TIME_WIDTH : integer := 13);
end     TEST_NG_1;
architecture MODEL of TEST_NG_1 is
begin
    process
        variable  text_line      : LINE;
        procedure p(T:in time;M:in string) is
        begin
            if    (TIME_WIDTH > 0) then
                WRITE(text_line, T, RIGHT,  TIME_WIDTH);
            elsif (TIME_WIDTH < 0) then
                WRITE(text_line, T, LEFT , -TIME_WIDTH);  -- Bounds check fail
                                                          -- here
            end if;
            WRITE(text_line, string'(" ") & M);
            WRITELINE(OUTPUT, text_line);
        end procedure;
    begin
        p(Now, string'("Simulation Start."));
        wait for 10 ns;
        p(Now, string'("Simulation Done."));
        wait;
    end process;
end MODEL;
