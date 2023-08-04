entity assert10 is
end entity;

use std.env.all;

architecture test of assert10 is
begin

    process is
        variable valid : boolean;
    begin
        SetVhdlAssertFormat(error, "{t} foo {s} bar {r:#<10} {r:#>10} {r:#^10}", valid);
        assert valid;
        assert false report "message";
        assert false report "long message";
        SetVhdlAssertFormat(error, "foo {Q} bar", valid);
        assert not valid;
        wait for 5 ps;
        SetVhdlAssertFormat(error, "** {S}: {r: <10} at {t:.ns} : {i}", valid);
        assert valid;
        assert false report "message";
        assert false report "message" severity warning;
        SetVhdlAssertFormat(error, "{t:.foobar}", valid);
        assert not valid;
        report "report" severity error;
        SetVhdlAssertFormat(warning, "{s:.sec}");  -- Aborts
        report "should not reach here" severity failure;
        wait;
    end process;

end architecture;
