use     std.textio.all;
entity  issue308 is
    generic (TIME_WIDTH : integer := 13);
end     issue308;
architecture MODEL of issue308 is
begin
    process
        variable  text_line      : LINE;
        procedure p(T:in time;M:in string) is
        begin
            if    (TIME_WIDTH > 0) then
                WRITE(text_line, T, RIGHT,  TIME_WIDTH);
            elsif (TIME_WIDTH < 0) then
                WRITE(text_line, T, LEFT , -TIME_WIDTH);
            end if;
            WRITE(text_line, string'(" ") & M);
            WRITELINE(OUTPUT, text_line);
        end procedure;
    begin
        p(Now, string'("Simulation Start."));
        wait for  100 ns;
        p(Now, string'("Simulation Phase 1."));
        wait for  100 ns;
        p(Now, string'("Simulation Phase 2."));
        wait for 1947 ns;
        p(Now, string'("Simulation Phase 3."));
        wait for    1 ns;
        p(Now, string'("Simulation Phase 4."));
        wait for    1 ns;
        p(Now, string'("Simulation Done."));
        wait;
    end process;
end MODEL;
