package genpack is
    generic (type t);

    type pt is protected
        impure function get return t;
        impure function get_str return string;
        procedure put (val : t);
    end protected;
end package;

package body genpack is
    constant str : string := "hello world";

    type pt is protected body
        variable cur : t;

        impure function get return t is
        begin
            return cur;
        end function;

        procedure put (val : t) is
        begin
            cur := val;
        end procedure;

        impure function get_str return string is
        begin
            return str;                 -- Accesses parent package
        end function;
    end protected body;
end package body;

-------------------------------------------------------------------------------

package testpack is
    package int_pack is new work.genpack
        generic map ( t => integer );
    use int_pack.all;
end package;

-------------------------------------------------------------------------------

entity issue943 is
end entity;

use work.testpack.all;

architecture test of issue943 is
    shared variable sv : int_pack.pt;
begin

    check: process is
    begin
        sv.put(5);
        assert sv.get = 5;
        assert sv.get_str = "hello world";

        wait;
    end process;

end architecture;
