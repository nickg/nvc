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
        wait;
    end process;

    p2: process is                      -- VHDL_VERSION, etc.
    begin
        assert vhdl_version = "2019";
        assert tool_type = "SIMULATION";
        report tool_version;
        wait;
    end process;

end architecture;
