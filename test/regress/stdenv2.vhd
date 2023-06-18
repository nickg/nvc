entity stdenv2 is
end entity;

use std.env.all;
use std.textio.all;

architecture test of stdenv2 is
begin

    p1: process is
        variable open_status : dir_open_status;
        variable create_status : dir_create_status;
        file f : text;
    begin
        report dir_workingdir;

        assert not dir_itemisdir("tmp")
            report "remove tmp directory first" severity failure;

        dir_createdir("tmp", false, create_status);
        assert create_status = STATUS_OK;
        assert dir_createdir("tmp") = STATUS_ITEM_EXISTS;
        assert dir_workingdir("tmp") = STATUS_OK;
        report dir_workingdir;
        dir_workingdir("/not/here", open_status);
        assert open_status = STATUS_NOT_FOUND;

        assert dir_itemexists(".");
        assert not dir_itemexists("file");

        file_open(f, "file", write_mode);
        file_close(f);

        assert dir_itemexists("file");

        assert dir_itemisfile("file");
        assert not dir_itemisfile("not.here");
        assert not dir_itemisfile(".");

        assert not dir_itemisdir("file");
        assert dir_itemisdir("..");

        wait;
    end process;

end architecture;
