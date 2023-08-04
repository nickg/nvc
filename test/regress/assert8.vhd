entity assert8 is
end entity;

use std.env.all;

architecture test of assert8 is
begin

    process is
    begin

        assert (IsVhdlAssertFailed          = false);
        assert (IsVhdlAssertFailed(note)    = false);
        assert (IsVhdlAssertFailed(warning) = false);
        assert (IsVhdlAssertFailed(error)   = false);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 0);
        assert (GetVhdlAssertCount(note)    = 0);
        assert (GetVhdlAssertCount(warning) = 0);
        assert (GetVhdlAssertCount(error)   = 0);
        assert (GetVhdlAssertCount(failure) = 0);


        assert (False) report "First note" severity note;

        assert (IsVhdlAssertFailed          = false);
        assert (IsVhdlAssertFailed(note)    = true);
        assert (IsVhdlAssertFailed(warning) = false);
        assert (IsVhdlAssertFailed(error)   = false);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 0);
        assert (GetVhdlAssertCount(note)    = 1);
        assert (GetVhdlAssertCount(warning) = 0);
        assert (GetVhdlAssertCount(error)   = 0);
        assert (GetVhdlAssertCount(failure) = 0);


        assert (False) report "First warning" severity warning;

        assert (IsVhdlAssertFailed          = true);
        assert (IsVhdlAssertFailed(note)    = true);
        assert (IsVhdlAssertFailed(warning) = true);
        assert (IsVhdlAssertFailed(error)   = false);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 1);
        assert (GetVhdlAssertCount(note)    = 1);
        assert (GetVhdlAssertCount(warning) = 1);
        assert (GetVhdlAssertCount(error)   = 0);
        assert (GetVhdlAssertCount(failure) = 0);


        assert (False) report "First error" severity error;

        assert (IsVhdlAssertFailed          = true);
        assert (IsVhdlAssertFailed(note)    = true);
        assert (IsVhdlAssertFailed(warning) = true);
        assert (IsVhdlAssertFailed(error)   = true);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 2);
        assert (GetVhdlAssertCount(note)    = 1);
        assert (GetVhdlAssertCount(warning) = 1);
        assert (GetVhdlAssertCount(error)   = 1);
        assert (GetVhdlAssertCount(failure) = 0);


        assert (False) report "Second note" severity note;

        assert (IsVhdlAssertFailed          = true);
        assert (IsVhdlAssertFailed(note)    = true);
        assert (IsVhdlAssertFailed(warning) = true);
        assert (IsVhdlAssertFailed(error)   = true);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 2);
        assert (GetVhdlAssertCount(note)    = 2);
        assert (GetVhdlAssertCount(warning) = 1);
        assert (GetVhdlAssertCount(error)   = 1);
        assert (GetVhdlAssertCount(failure) = 0);


        assert (False) report "Second warning" severity warning;

        assert (IsVhdlAssertFailed          = true);
        assert (IsVhdlAssertFailed(note)    = true);
        assert (IsVhdlAssertFailed(warning) = true);
        assert (IsVhdlAssertFailed(error)   = true);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 3);
        assert (GetVhdlAssertCount(note)    = 2);
        assert (GetVhdlAssertCount(warning) = 2);
        assert (GetVhdlAssertCount(error)   = 1);
        assert (GetVhdlAssertCount(failure) = 0);


        assert (False) report "Second error" severity error;

        assert (IsVhdlAssertFailed          = true);
        assert (IsVhdlAssertFailed(note)    = true);
        assert (IsVhdlAssertFailed(warning) = true);
        assert (IsVhdlAssertFailed(error)   = true);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 4);
        assert (GetVhdlAssertCount(note)    = 2);
        assert (GetVhdlAssertCount(warning) = 2);
        assert (GetVhdlAssertCount(error)   = 2);
        assert (GetVhdlAssertCount(failure) = 0);


        report "Clearing VHDL asserts";
        ClearVhdlAssert;

        assert (IsVhdlAssertFailed          = false);
        assert (IsVhdlAssertFailed(note)    = false);
        assert (IsVhdlAssertFailed(warning) = false);
        assert (IsVhdlAssertFailed(error)   = false);
        assert (IsVhdlAssertFailed(failure) = false);

        assert (GetVhdlAssertCount          = 0);
        assert (GetVhdlAssertCount(note)    = 0);
        assert (GetVhdlAssertCount(warning) = 0);
        assert (GetVhdlAssertCount(error)   = 0);
        assert (GetVhdlAssertCount(failure) = 0);


        report "Note report" severity note;
        report "Warning report" severity warning;
        report "Error report" severity error;

        assert (GetVhdlAssertCount          = 2);
        assert (GetVhdlAssertCount(note)    = 1);
        assert (GetVhdlAssertCount(warning) = 1);
        assert (GetVhdlAssertCount(error)   = 1);
        assert (GetVhdlAssertCount(failure) = 0);

        -- TODO: How to test failure severity when it always stops simulation?

        finish;
    end process;

end architecture;
