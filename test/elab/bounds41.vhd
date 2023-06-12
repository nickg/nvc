package pack is
    type rec is record
        f : integer_vector;
    end record;

    type rec_array is array (natural range <>) of rec;

    type rec2 is record
        a : rec_array;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub2 is
    port ( p : out rec2(a(1 to 2)(f(1 to 3))) );
end entity;

architecture test of sub2 is
begin
    p.a(2).f <= (others => 1);
end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity bounds41 is
end entity;

architecture test of bounds41 is
    signal s2 : rec2(a(1 to 1)(f(1 to 3)));
begin

    u2: entity work.sub2
        port map (s2);                  -- Error

end architecture;
