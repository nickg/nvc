use std.textio.all;

package pkg is
    impure function f1(
        path : string
    ) return string;

    impure function f1(
        file f : text
    ) return string;
end package pkg;

package body pkg is
    impure function f1(
        path : string
    ) return string is
        file f : text open read_mode is path;
    begin
        return f1(f);
    end function;
    impure function f1(
        file f : text
    ) return string is
        variable ef : boolean := false;

    begin
        ef := endfile(f);
        if (ef = TRUE) then
            return "true";
        else
            return "false";
        end if;
    end function;

end package body pkg;

-------------------------------------------------------------------------------

use std.textio.all;

library work;
use work.pkg.all;

entity issue1229 is
end entity;

architecture RTL of issue1229 is
    procedure test is
        constant STRING_INPUT : string := f1("file.txt");
    begin
        assert STRING_INPUT = "true";
    end procedure;
begin

    process is
        file f : text;
    begin
        file_open(f, "file.txt", WRITE_MODE);
        file_close(f);

        test;
        wait;
    end process;

end architecture RTL;
