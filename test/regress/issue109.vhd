package pack is
    procedure proc (x : out integer);
end package;

package body pack is

    procedure proc (x : out integer) is
    begin
        x := 5;
    end procedure;

end package body;

-------------------------------------------------------------------------------

entity issue109 is
end entity;

use work.pack.all;

architecture test of issue109 is

    function func return integer is
        variable r : integer;
    begin
        proc(r);
        return r;
    end function;

begin

    process is
    begin
        assert func = 5;
        wait;
    end process;

end architecture;
