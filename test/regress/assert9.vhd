entity assert9 is
end entity;

use std.env.all;

architecture test of assert9 is
begin

    process is
    begin

        assert (GetVhdlAssertEnable(note))          report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(warning))       report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(error))         report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(failure))       report "FAIL" severity error;
        wait for 1 ns;

        -- Try disabling step by step
        SetVhdlAssertEnable(note, false);
        assert (not GetVhdlAssertEnable(note))      report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(warning))       report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(error))         report "FAIL" severity failure;
        wait for 1 ns;

        SetVhdlAssertEnable(warning, false);
        assert (not GetVhdlAssertEnable(note))      report "FAIL" severity failure;
        assert (not GetVhdlAssertEnable(warning))   report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(error))         report "FAIL" severity failure;
        wait for 1 ns;

        SetVhdlAssertEnable(error, false);
        assert (not GetVhdlAssertEnable(note))      report "FAIL" severity failure;
        assert (not GetVhdlAssertEnable(warning))   report "FAIL" severity failure;
        assert (not GetVhdlAssertEnable(error))     report "FAIL" severity failure;
        wait for 1 ns;

        -- Try Re-enabling everything
        SetVhdlAssertEnable(true);
        assert (GetVhdlAssertEnable(note))          report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(warning))       report "FAIL" severity failure;
        assert (GetVhdlAssertEnable(error))         report "FAIL" severity failure;
        wait for 1 ns;

        -- Check all reports but failure are thrown
        SetVhdlAssertEnable(failure, false);
        report "EXAMPLE NOTE"    severity note;
        report "EXAMPLE WARNING" severity warning;
        report "EXAMPLE ERROR"   severity error;
        report "EXAMPLE FAILURE" severity failure;
        wait for 1 ns;

        -- Disable one-by-one and make sure it is the only one not shown
        SetVhdlAssertEnable(note, false);
        report "EXAMPLE NOTE"    severity note;
        report "EXAMPLE WARNING" severity warning;
        report "EXAMPLE ERROR"   severity error;
        report "EXAMPLE FAILURE" severity failure;
        wait for 1 ns;

        SetVhdlAssertEnable(warning, false);
        report "EXAMPLE NOTE"    severity note;
        report "EXAMPLE WARNING" severity warning;
        report "EXAMPLE ERROR"   severity error;
        report "EXAMPLE FAILURE" severity failure;
        wait for 1 ns;

        SetVhdlAssertEnable(error, false);
        report "EXAMPLE NOTE"    severity note;
        report "EXAMPLE WARNING" severity warning;
        report "EXAMPLE ERROR"   severity error;
        report "EXAMPLE FAILURE" severity failure;
        wait for 1 ns;

        -- Re-enable just failure to kill the test and check it
        SetVhdlAssertEnable(failure, true);
        report "END THE TEST" severity failure;

    end process;

end architecture;
