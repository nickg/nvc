package pack is
    procedure proc(address : bit_vector(7 downto 0); value : integer);
    alias proc_alias is proc [bit_vector, integer];
end package;

-------------------------------------------------------------------------------

entity vunit9 is
end entity;

use work.pack;

architecture test of vunit9 is
begin

    p1: process is
    begin
        pack.proc_alias(value => 5, address => X"01");  -- OK
        wait;
    end process;

end architecture;
