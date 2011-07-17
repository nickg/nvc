package pack1 is
    type my_int1 is range 0 to 10;
end package;

-------------------------------------------------------------------------------

package pack2 is
    type my_int1 is range 0 to 10;
end package;

-------------------------------------------------------------------------------

use work.pack1;
use work.pack2;    

entity no_use_clause is
    port (
        a : in pack1.my_int1;
        b : out pack2.my_int1 );
end entity;

-------------------------------------------------------------------------------

architecture a of no_use_clause is
    type my_int1 is range 10 to 50;
begin

    process is
    begin
        -- Should fail as types have same name but from different packages
        b <= a;
    end process;

    process is
        variable v : pack2.my_int1;
    begin
        b <= v;                         -- OK
    end process;

    process is
        variable v : my_int1;
    begin
        -- Should fail as local my_int1 distinct from pack1.my_int1
        v := a;
    end process;

end architecture;

-------------------------------------------------------------------------------

