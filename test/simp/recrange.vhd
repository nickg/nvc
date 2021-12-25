package pack is

    type rec is record
        x : bit_vector(1 to 8);
    end record;

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( r : in rec );
end entity;

architecture test of sub is
begin

    p1: process (r)
    begin
        assert r.x(r.x'range) = (r.x'range => '0');
    end process;

end architecture;
