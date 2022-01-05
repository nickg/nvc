package pack is
    function resolved (x : bit_vector) return bit;
    subtype rbit is resolved bit;
end package;

use work.pack.all;

entity sub is
    port ( x : buffer rbit );
end entity;

architecture test of sub is
begin

    x <= '1';
    x <= '0';                           -- Error

end architecture;

entity buffer1 is
end entity;

use work.pack.all;

architecture test of buffer1 is
    signal x : rbit;
begin

    uut: entity work.sub port map ( x );

end architecture;
