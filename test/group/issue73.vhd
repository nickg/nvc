entity issue73 is
end entity;

architecture test of issue73 is
    type ma_t is array (1 downto 0) of bit_vector(3 downto 0);
    signal x : ma_t;
begin

    x(0)(1 downto 0) <= "00";
    x(0)(3 downto 2) <= "11";
    x(1)(3 downto 0) <= "0101";

end architecture;
