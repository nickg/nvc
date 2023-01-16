package pack is

    type pt is protected
        procedure increment;
        procedure add (other : inout pt);
        impure function get return integer;
    end protected;

    procedure increment (p : inout pt);

end package;

package body pack is

    type pt is protected body
        variable val : integer := 0;

        procedure increment is
        begin
            val := val + 1;
        end procedure;

        impure function get return integer is
        begin
            return val;
        end function;

        procedure add (other : inout pt) is
        begin
            val := val + other.get;
        end procedure;

    end protected body;

    procedure increment (p : inout pt) is
    begin
        p.increment;
    end procedure;

    procedure add (p, q : inout pt) is
    begin
        p.add(q);
    end procedure;

end package body;

-------------------------------------------------------------------------------

entity issue589 is
end entity;

use work.pack.all;

architecture test of issue589 is
    shared variable p, q : pt;
begin

    p1: process is
    begin
        increment(p);
        wait for 1 ns;
        increment(p);
        wait for 1 ns;
        assert p.get = 2;
        q.add(p);
        wait for 1 ns;
        assert q.get = 2;
        wait;
    end process;

end architecture;
