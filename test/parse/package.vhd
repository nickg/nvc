package one is
    type my_int is range 0 to 100;

    function add_one(x : my_int) return my_int;
end package;

-------------------------------------------------------------------------------

use work.one.all;

package two is
    subtype my_int2 is work.one.my_int range 10 to 50;
end package two;

-------------------------------------------------------------------------------

package body one is

    function add_one(x : my_int) return my_int is
        variable x : integer;
    begin
        null;
    end function add_one;

    shared variable x : integer;

end package body;

-------------------------------------------------------------------------------

library foo;

package three is

    signal s : integer;

end package;
