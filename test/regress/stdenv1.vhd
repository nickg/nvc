entity stdenv1 is
end entity;

use std.env.all;
use std.textio.all;

architecture test of stdenv1 is
begin

    p1: process is                      -- GETENV
        variable l : line;
    begin
        report getenv("PATH");
        report getenv("HOME");
        assert getenv("FOO") = "123";
        l := getenv("FOO");
        assert l.all = "123";
        assert getenv("TOOL_TYPE") = TOOL_TYPE;
        assert getenv("VHDL_VERSION") = VHDL_VERSION;
        assert getenv("NOT_HERE") = "";
        l := getenv("NOT_HERE");
        assert l = null;
        wait;
    end process;

    p2: process is                      -- VHDL_VERSION, etc.
    begin
        assert vhdl_version = "2019";
        assert tool_type = "SIMULATION";
        report tool_version;
        wait;
    end process;

    p3: process is                      -- EPOCH, time functions
        variable tr : time_record;
    begin
        report "time: " & to_string(epoch);
        assert epoch > 1660068924.0;    -- 9th August 2022
        assert epoch < 4294967396.0;    -- 19th January 2038

        tr := localtime;
        report to_string(tr);
        assert tr.year >= 2022;

        tr := gmtime;
        report to_string(tr, 3);
        assert tr.year >= 2022;

        wait;
    end process;

end architecture;
