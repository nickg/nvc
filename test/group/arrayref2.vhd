entity arrayref2 is
end entity;

architecture test of arrayref2 is
    type i2d is array (integer range <>, integer range <>) of integer;
    type bv2d is array (integer range <>, integer range <>) of bit_vector(1 downto 0);

    signal x : i2d(0 to 2, 0 to 2);     -- 0..8
    signal y : bv2d(0 to 2, 0 to 2);    -- 9..26
begin

    x(1, 1) <= 1;
    x(2, 1) <= 1;

    y(1, 1) <= "00";
    y(2, 1) <= "11";

end architecture;
