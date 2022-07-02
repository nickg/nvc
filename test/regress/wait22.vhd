-- Test case from Brian Padalino
--
entity wait22 is end entity ;

architecture arch of wait22 is

    procedure generate_clock(signal ena : in boolean ; signal clock : inout bit) is
    begin
        -- Inspired by UVVM clock_generator procedure
        loop
            if not ena then
                if now /= 0 ps then
                    report "Stopping clock" ;
                end if ;
                clock <= '0' ;
                wait until ena ;
            end if ;
            clock <= '1' ;
            wait for 5 ns ;
            clock <= '0' ;
            wait for 5 ns ;
        end loop ;
    end procedure ;

    signal clock_enable :   boolean := false ;
    signal clock    :   bit := '0' ;
    signal reset    :   bit := '1' ;

begin

    gen_clock_p: generate_clock(clock_enable, clock) ;

    tick_p: process(clock, reset)
    begin
        if reset = '1' then
        else
            if clock'event and clock = '1' then
                report "Clock tick" ;
            end if ;
        end if ;
    end process ;

    tb : process
    begin
        report "About to begin" ;
        wait for 10 ns ;
        reset <= '0' ;
        wait for 10 ns ;
        clock_enable <= true ;
        wait for 100 ns ;
        std.env.stop;
    end process ;

end architecture ;
