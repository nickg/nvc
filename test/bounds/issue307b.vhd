-- test_ng_2.vhd
use     std.textio.all;
entity  TEST_NG_2 is
end     TEST_NG_2;
architecture MODEL of TEST_NG_2 is
    constant TIME_WIDTH : integer := 13;
begin
    process
        variable  text_line      : LINE;
        procedure p(T:in time;M:in string) is
        begin
            if    (TIME_WIDTH > 0) then
                WRITE(text_line, T, RIGHT, ABS(TIME_WIDTH));  -- Bogus bounds
                                                              -- check failure
            elsif (TIME_WIDTH < 0) then
                WRITE(text_line, T, LEFT , ABS(TIME_WIDTH));
            end if;
            WRITE(text_line, string'(" ") & M);
            WRITELINE(OUTPUT, text_line);
        end procedure;
    begin
        p(Now, string'("Simulation Start."));
        wait for 10 ns;
        p(Now, string'("Simulation Done."));
        assert FALSE report "Simulation complete." severity FAILURE;
    end process;
end MODEL;
