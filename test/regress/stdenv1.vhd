entity stdenv1 is
end entity;

use std.env.all;
use std.textio.all;

architecture test of stdenv1 is
begin

    p1: process is
        variable l : line;
    begin
        report getenv("PATH");
        report getenv("HOME");
        assert getenv("FOO") = "123";
        l := getenv("FOO");
        assert l.all = "123";
        wait;
    end process;

end architecture;
