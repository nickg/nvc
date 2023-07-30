entity stdenv6 is
end entity;

use std.env.all;
use std.textio.all;

architecture test of stdenv6 is
begin

    p1: process is
        file f : text;
        variable status : file_delete_status;
    begin
        file_open(f, "tmp.txt", write_mode);
        file_close(f);

        assert dir_itemexists("tmp.txt");

        dir_deletefile("tmp.txt", status);
        assert status = STATUS_OK;

        assert not dir_itemexists("tmp.txt");

        assert dir_deletefile("tmp.txt") = STATUS_NO_FILE;

        wait;
    end process;

end architecture;
