entity array4 is
end entity;

architecture test of array4 is
    type ma_t is array (1 downto 0, 7 downto 0) of bit_vector(7 downto 0);
    signal ma : ma_t;
begin

    process is
    begin
        ma <= (others => (others => (others => '0')));
        wait for 1 ns;
        assert ma(1, 2) = X"00";
        wait;
    end process;

end architecture;
