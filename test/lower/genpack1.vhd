package pack is
    generic ( n : integer ) ;
    type t is (a, b, c);
end package ;

-------------------------------------------------------------------------------

package p5 is new work.pack generic map ( 5 );

entity genpack1 is
end entity;

use work.p5.all;

architecture test of genpack1 is
    signal s : t;
begin

    p1: process is
    begin
        assert t'image(s) = "a";
        wait;
    end process;

end architecture;
