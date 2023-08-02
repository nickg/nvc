entity stdenv6 is
end entity;

use std.env.all;
use std.textio.all;

architecture test of stdenv6 is
begin

    p1: process is
        file f : text;
        variable status : file_delete_status;
        variable dir : directory;
        variable found : boolean;
    begin
        file_open(f, "tmp.txt", write_mode);
        file_close(f);

        assert dir_itemexists("tmp.txt");

        assert dir_open(dir, ".") = STATUS_OK;

        for i in dir.items'range loop
            report dir.items(i).all;
            if dir.items(i).all = "tmp.txt" then
                assert not found;
                found := true;
            end if;
        end loop;

        assert found report "missing tmp.txt";

        report dir.name.all;

        dir_close(dir);

        dir_deletefile("tmp.txt", status);
        assert status = STATUS_OK;

        assert not dir_itemexists("tmp.txt");

        assert dir_deletefile("tmp.txt") = STATUS_NO_FILE;

        assert dir_createdir("testdir") = STATUS_OK;
        assert dir_itemisdir("testdir");
        assert dir_deletedir("testdir") = STATUS_OK;

        assert dir_deletedir("nothere") = STATUS_NO_DIRECTORY;

        assert dir_createdir("testdir") = STATUS_OK;
        file_open(f, "testdir/tmp.txt", write_mode);
        file_close(f);
        assert dir_createdir("testdir/sub") = STATUS_OK;
        file_open(f, "testdir/sub/sub.txt", write_mode);
        file_close(f);
        assert dir_deletedir("testdir") = STATUS_NOT_EMPTY;

        assert dir_deletedir("testdir", recursive => true) = STATUS_OK;
        assert not dir_itemexists("testdir");

        wait;
    end process;

end architecture;
