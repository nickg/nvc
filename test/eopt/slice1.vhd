entity slice1 is
end entity;

architecture test of slice1 is
    signal x : bit_vector(0 to 7);
    signal y : bit_vector(7 downto 0);
begin

    x(0 to 3) <= "1111";
    x(4 to 2) <= (others => '0');
    x(4 to 5) <= "00";
    x(5 to 7) <= "111";

    y(3 downto 0) <= "1111";
    y(2 downto 4) <= (others => '0');
    y(5 downto 4) <= "00";
    y(7 downto 5) <= "111";

end architecture;
