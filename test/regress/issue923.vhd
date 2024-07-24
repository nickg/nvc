package pack is
    generic (type t);

    type pt is protected
        impure function get_path return string;
    end protected;

    impure function get_path return string;
end package;

package body pack is
    type pt is protected body
        impure function get_path return string is
        begin
            return pt'path_name;
        end function;
    end protected body;

    shared variable sv : pt;

    impure function get_path return string is
    begin
        return sv.get_path;
    end function;
end package body;

-------------------------------------------------------------------------------

package pack_int is new work.pack generic map (integer);

-------------------------------------------------------------------------------

entity issue923 is
end entity;

use work.pack_int.all;

architecture test of issue923 is
begin

    check: process is
    begin
        report get_path;
        assert get_path = ":work:pack_int:sv:";
        wait;
    end process;

end architecture;
