package pack is
    procedure p;
end package;

package body pack is
    procedure p is
        alias s1 is << signal .ename6.b1.s1 : bit >>;
    begin
        s1 <= force '1';
    end procedure;
end package body;

-------------------------------------------------------------------------------

entity ename6 is
end entity;

use work.pack.all;

architecture test of ename6 is
begin

    b1: block is
        signal s1 : bit;
        constant c1 : string := "hello";
    begin
        p1: process is
        begin
            assert s1 = '0';
            p;
            wait for 0 ns;
            assert s1 = '1';
            s1 <= release;
            wait for 6 ns;
            assert s1 = '1';
            wait;
        end process;
    end block;

    p1: process is
    begin
        wait for 5 ns;
        p;
        wait;
    end process;

end architecture;
