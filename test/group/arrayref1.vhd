entity arrayref is
end entity;

architecture test of arrayref is
    type bv2d is array (integer range <>) of bit_vector(1 downto 0);

    signal x : bit_vector(2 downto 0);  -- 0..2
    signal y : bit_vector(1 downto 0);  -- 3..4
    signal i : integer;                 -- 5..5
    signal p : bv2d(0 to 1);            -- 6..9
    signal q : bv2d(0 to 1);            -- 10..13
begin

    x(0) <= '1';
    x(1) <= '0';

    y(i) <= '1';

    p(0)(i) <= '1';
    p(1) <= "00";

    q(i) <= "10";

end architecture;
