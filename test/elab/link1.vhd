package pack is
    constant k : integer := 1 / 0;                 -- Error

    function is_even (x : integer) return boolean;
end package;

package body pack is
    function is_even (x : integer) return boolean is
    begin
        return (x rem 2) = 0;
    end function;
end package body;

-------------------------------------------------------------------------------

entity link1 is
end entity;

use work.pack.all;

architecture test of link1 is
begin

    g: if is_even(4) generate
    begin
        p1: process is
        begin
            report "hello";
            wait;
        end process;
    end generate;

end architecture;
