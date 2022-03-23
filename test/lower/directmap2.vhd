package directmap2_pack is
    type rec is record
        x : integer;
        y : bit;
        z : bit_vector(1 to 3);
    end record;
end package;

-------------------------------------------------------------------------------

use work.directmap2_pack.all;

entity bot is
    port ( r : in rec );
end entity;

architecture test of bot is
begin

end architecture;

-------------------------------------------------------------------------------

use work.directmap2_pack.all;

entity directmap2 is
end entity;

architecture test of directmap2 is
    signal p : bit;
    signal q : bit_vector(1 to 3);
    signal i : integer;
begin

    uut: entity work.bot
        port map (
            r.y => p,
            r.z => q,
            r.x => i );

end architecture;
