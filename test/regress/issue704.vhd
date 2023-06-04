package pack is
    type rec_t is record
        x, y : natural;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( i : in rec_t; o : out rec_t );
end entity;

architecture test of sub is
begin
    o <= i after 1 ns;
end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity issue704 is
end entity;

architecture test of issue704 is
    signal x, y : rec_t;
begin

    u: entity work.sub port map ( x, y );

    x <= (1, 2) after 1 ns, (5, 6) after 2 ns;

end architecture;
