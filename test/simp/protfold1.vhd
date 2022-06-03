package pack is

    type pt is protected
        procedure not_called;
    end protected;

    function add2(x : integer) return integer;

end package;

package body pack is

    type pt is protected body
        procedure not_called is
        begin
        end procedure;
    end protected body;

    shared variable p : pt;

    function add2(x : integer) return integer is
    begin
        return x + 2;
    end function;

end package body;

-------------------------------------------------------------------------------

entity protfold1 is
end entity;

use work.pack.all;

architecture test of protfold1 is
begin

    g0: if add2(4) = 6 generate
    begin
        p1: process is
        begin
            report "ok";
            wait;
        end process;
    end generate;

end architecture;
