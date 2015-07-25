package pack is
    function func(x : string) return boolean;
end package;

package body pack is

    function func2(x : string) return boolean;

    function func(x : string) return boolean is
    begin
        return func2(x);
    end function;

    function func2(x : string) return boolean is
        variable r : boolean;
    begin
        if x = "StratixHC" then
            r := true;
        end if;
        return r;
    end function;

end package body;

-------------------------------------------------------------------------------

entity issue212 is
    generic (
        device : string := "StratixHC" );
end entity;

use work.pack.all;

architecture test of issue212 is
    signal s : bit;
begin

    gen: if func(device) generate
        s <= '1';
    end generate;

end architecture;
