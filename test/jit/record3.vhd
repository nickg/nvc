package pack5 is
    type int_vector is array (natural range <>) of integer;

    type pair is record
        first  : integer;
        second : integer;
    end record;

    type pair_vector is array (natural range <>) of pair;

    type rec is record
        x : integer;                    -- 0
        y : integer;                    -- 4
        a : int_vector(1 to 3);         -- 8
        b : pair_vector(1 to 2);        -- 24
        z : integer;                    -- 40
    end record;

    constant r : rec;
end package;

package body pack5 is
    constant r : rec := (1, 2, (3, 4, 5), ((6, 7), (8, 9)), 10);
end package body;

-------------------------------------------------------------------------------

package pack6 is
    function sum_fields return integer;
end package;

use work.pack5.all;

package body pack6 is
    function sum_fields return integer is
        variable sum : integer := r.x + r.y + r.z;
    begin
        for i in 1 to 3 loop
            sum := sum + r.a(i);
        end loop;
        for i in 1 to 2 loop
            sum := sum + r.b(i).first + r.b(i).second;
        end loop;
        return sum;
    end function;
end package body;
