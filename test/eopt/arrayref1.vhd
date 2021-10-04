entity arrayref is
end entity;

architecture test of arrayref is
    function resolved (x : bit_vector) return bit;

    subtype r_bit is resolved bit;
    type r_bit_vector is array (natural range <>) of r_bit;

    type bv2d is array (integer range <>) of r_bit_vector(1 downto 0);

    signal x : r_bit_vector(2 downto 0);  -- 0..2
    signal y : r_bit_vector(1 downto 0);  -- 3..4
    signal i : integer;                   -- 5..5
    signal p : bv2d(0 to 1);              -- 6..9
    signal q : bv2d(0 to 2);              -- 10..15
    signal r : bv2d(0 to 2);              -- 16..21
begin

    x(0) <= '1';
    x(1) <= '0';

    y(i) <= '1';

    p(0)(i) <= '1';
    p(1) <= "00";

    q(i) <= "10";

    r(2)(i) <= '1';

end architecture;
