package pack is
    impure function func (x : integer) return integer;
end package;

package body pack is
    impure function func (x : integer) return integer is
    begin
        report "A ~ " & x'instance_name;
        report "B ~ " & func'instance_name;
        return x + 1;
    end function;
end package body;

-------------------------------------------------------------------------------

entity attr17 is end entity ;

use work.pack.all;

architecture arch of attr17 is

    procedure report_instance_name(x : in string) is
        constant val : integer := 0 ;
    begin
        report "C ~ " & val'instance_name ;
    end procedure ;

begin

    tb : process
        variable x : integer;
    begin
        x := func(x);
        report_instance_name("something");
        wait;
    end process ;

end architecture ;
