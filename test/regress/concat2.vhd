entity concat2 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of concat2 is
    type int_array is array (integer range <>) of integer;
    
    function get_array return int_array is
    begin
        return (1, 2, 3);
    end function;

    function pad(a : in int_array) return int_array is
    begin
        return (0, 0) & a;
    end function;
    
begin

    process is
        variable x : int_array(0 to 3);
    begin
        x := get_array & ( 0 => 4 );
        assert x = (1, 2, 3, 4);
        x := get_array & ( 6 => 8 );
        assert x = (1, 2, 3, 8);
        x := pad((1, 2));
        assert x = (0, 0, 1, 2);
        wait;
    end process;

end architecture;
