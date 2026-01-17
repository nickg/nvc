use std.env.all ;

entity issue1381 is
end entity ;

architecture arch of issue1381 is

begin

    tb : process
        variable tr1, tr2, tr3 : std.env.time_record ;
        variable d1, d2 : real ;
    begin
        -- Grab the localtime
        tr1 := localtime ;

        -- Wait a delta cycle maybe?
        wait for 0 ns ;

        -- Report the time
        report to_string(tr1) & '.' & to_string(tr1.microsecond); -- Currently printing 2026-00-16T15:04:10.134101 but I think the month needs to be +1

        -- Grab the localtime again
        wait for 0 ns ;
        tr2 := localtime ;

        -- Report the time
        report to_string(tr2) & '.' & to_string(tr2.microsecond);

        -- Deltas
        d1 := tr2 - tr1 ; -- Should be positive since time is monotonically increasing
        d2 := tr1 - tr2 ; -- Should be negative since time is monotonically increasing
        report "d1: " & to_string(d1) & ", d2: " & to_string(d2) ;

        -- d1 should be positive or zero
        assert d1 >= 0.0 report "d1 needs to be >= 0 but it isn't: " & to_string(d1) severity error ;
        assert d2 <= 0.0 report "d2 neesd to be <= 0 but it isn't: " & to_string(d2) severity error ;

        tr3 := (832078,39,35,10,17,0,2026,saturday,16);
        assert to_string(tr3, 6) = "2026-01-17T10:35:39.832078";

        std.env.stop ;
    end process ;

end architecture ;
