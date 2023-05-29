package pack is
    type pair is record
        x, y : integer;
    end record;

    type pair_array is array (natural range <>) of pair;
end package;

-------------------------------------------------------------------------------

entity sub is
end entity;

use work.pack.all;

architecture test of sub is
    type rec is record
        p : pair;
        f : bit;
    end record;

    signal r : rec;
begin

    r <= ((1, 2), '1') after 1 ns, ((5, 55), '0') after 5 ns;

end architecture;

-------------------------------------------------------------------------------

entity issue703 is
end entity;

use work.pack.all;

architecture test of issue703 is
    signal s : bit := '1';
    signal t : pair_array(1 to 3);
begin

    s <= not s after 2 ns when s = '1';

    u: entity work.sub;

end architecture;
