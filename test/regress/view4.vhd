package pack is
    function resolved (x : bit_vector) return bit;

    subtype rbit_vector is (resolved) bit_vector;

    type vec_t is record
        p : bit_vector(1 to 3);
        q : rbit_vector(1 to 3);
    end record;

    view vec_in_view of vec_t is
        p : in;
        q : inout;
    end view;

    type rec_t is record
        x, y : natural;
        z : vec_t;
    end record;

    view in_view of rec_t is
        x : in;
        y : out;
        z : view vec_in_view;
    end view;

    alias out_view is in_view'converse;
end package;

package body pack is
    function resolved (x : bit_vector) return bit is
        variable r : bit := '0';
    begin
        for i in x'range loop
            r := r or x(i);
        end loop;
        return r;
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( i : view in_view; o : view out_view );
end entity;

architecture test of sub is
    function "not" (v : vec_t) return vec_t is
    begin
        return (not v.p, not v.q);
    end function;
begin

    o.x <= i.x + 1;
    i.y <= o.y + 5;
    o.z <= not i.z;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity view4 is
end entity;

architecture test of view4 is
    signal i, o : rec_t;
begin

    u: entity work.sub port map (i, o);

    check: process is
    begin
        assert i = (0, 0, ("000", "000"));
        assert o = (0, 0, ("000", "000"));
        wait for 0 ns;
        assert i = (0, 5, ("000", "000"));
        assert o = (1, 0, ("111", "111"));
        i.x <= 7;
        o.y <= 12;
        i.z.p <= "101";
        i.z.q <= "001";
        wait for 1 ns;
        assert i = (7, 17, ("101", "001"));
        assert o = (8, 12, ("010", "110"));
        wait;
    end process;

end architecture;
