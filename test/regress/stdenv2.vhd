entity stdenv2 is
end entity;

use std.env.all;

architecture test of stdenv2 is
begin

    p1: process is
        variable open_status : dir_open_status;
        variable create_status : dir_create_status;
    begin
        report dir_workingdir;
        dir_createdir("tmp", false, create_status);
        assert create_status = STATUS_OK;
        assert dir_createdir("tmp") = STATUS_ITEM_EXISTS;
        assert dir_workingdir("tmp") = STATUS_OK;
        report dir_workingdir;
        dir_workingdir("/not/here", open_status);
        assert open_status = STATUS_NOT_FOUND;
        wait;
    end process;

end architecture;
