package pack1 is
    procedure read ( x : out integer );
end package;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    alias read is work.pack1.read [integer];
end package;

-------------------------------------------------------------------------------

use work.pack1.all;
use work.pack2.all;

entity test is
end entity;

architecture test of test is
begin
    process is
        variable x : integer;
    begin
        read(x);                        -- OK
        wait;
    end process;
end architecture;
