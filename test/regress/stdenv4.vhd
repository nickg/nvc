entity stdenv4 is
end entity;

use std.env.all;

architecture test of stdenv4 is

    procedure check_real (have, expect : real; tolerance : real := 0.00001) is
    begin
        assert have > expect - tolerance;
        assert have < expect + tolerance;
    end procedure;

begin

    p1: process is
        variable r : real := 1.0;
    begin
        check_real(time_to_seconds(now), 0.0);
        check_real(time_to_seconds(now + 1 ms), 0.001);
        check_real(time_to_seconds(now + 1 hr), 3600.0);
        check_real(time_to_seconds(now + 2.5 hr), 9000.0);
        assert seconds_to_time(r) = 1 sec;
        assert seconds_to_time(r / 2.0) = 500 ms;
        wait;
    end process;

end architecture;
