package pack is
    function nested(x : integer) return integer;
end package;

package body pack is

    function nested(x : integer) return integer is
        variable result : integer;

        procedure add1 is
        begin
            result := result + 1;
        end procedure;
    begin
        result := x;
        add1;
        add1;
        return result;
    end function;

end package body;

entity ffold2 is
end entity;

use work.pack.all;

architecture test of ffold2 is
begin

    b1: block is
        constant c1 : integer := nested(1);  -- 3
    begin
    end block;


end architecture;
