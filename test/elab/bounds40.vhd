package pack is
    type rec is record
        f : integer_vector;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( p : out rec(f(1 to 8)) );
end entity;

architecture test of sub is
begin
    p.f <= (others => 1);
end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity bounds40 is
end entity;

architecture test of bounds40 is
    signal s : rec(f(1 to 3));
begin

    u: entity work.sub
        port map (s);                   -- Error

end architecture;
