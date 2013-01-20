package pack is

    function add4(x : in integer) return integer;

end package;

package body pack is

    function add4(x : in integer) return integer is
    begin
        return x + 4;
    end function;

end package body;

-------------------------------------------------------------------------------

entity ffold is
end entity;

use work.pack.all;

architecture a of ffold is

    function add1(x : in integer) return integer is
    begin
        return x + 1;
    end function;

    function log2(x : in integer) return integer is
        variable r : integer := 0;
        variable c : integer := 1;
    begin
        --while true loop
        --end loop;
        if x <= 1 then
            r := 1;
        else
            while c < x loop
                r := r + 1;
                c := c * 2;
            end loop;
        end if;
        return r;
    end function;

    signal s1 : integer := add1(5);
    signal s2 : integer := add4(1);
    signal s3 : integer := log2(11);
    signal s4 : integer := log2(integer(real'(5.5)));

begin

end architecture;
