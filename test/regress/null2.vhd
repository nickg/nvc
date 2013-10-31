entity null2 is
end entity;

architecture test of null2 is
    signal x : bit_vector(0 downto 1);
begin

    process is
    begin
        x <= (others => '0');
        assert x'length = 0;
        wait;
    end process;

end architecture;
