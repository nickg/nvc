package pack is

    type int_vec is array (natural range <>) of integer;

    type rec is record
        data : int_vec(1 to 100);
    end record;

    impure function make_rec return rec;

    constant r : rec := make_rec;
    constant s : string := "hello";

    function get_string return string;

end package;

package body pack is

    impure function make_rec return rec is
        variable r : rec;
    begin
        for i in 1 to 100 loop
            for j in 1 to 100 loop
                r.data(j) := r.data(j) + 1;
            end loop;
        end loop;
        return r;
    end function;

    function get_string return string is
    begin
        return s;
    end function;

end package body;

-------------------------------------------------------------------------------

entity link4 is
end entity;

use work.pack.all;

architecture test of link4 is
begin

    p1: process is
    begin
        assert get_string = s;  -- OK
        assert get_string = s;  -- OK (failed spuriously)
        wait;
    end process;

end architecture;
