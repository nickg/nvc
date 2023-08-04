entity assert11 is
end entity;

use std.env.all;
use std.textio.all;

architecture test of assert11 is
begin

    process is
        variable l : line;
        variable i : integer;
    begin
        assert GetVhdlReadSeverity = error;
        SetVhdlReadSeverity(warning);
        l := new string'("hello");
        assert GetVhdlReadSeverity = warning;
        assert GetVhdlAssertCount(warning) = 0;
        read(l, i);
        assert GetVhdlAssertCount(warning) = 1;
        wait;
    end process;

end architecture;
