package pack is
    signal x : natural;
    signal y : bit_vector(2 downto 0);
end package;

entity issue901 is
end entity;

use work.pack.all;

architecture test of issue901 is
    signal z : bit;
begin

    x <= 1 after 1 ns, 5 after 2 ns;

    z <= y(1) after 1 ns;

    y <= "101" after 6 ns, "110" after 7 ns;

end architecture;
