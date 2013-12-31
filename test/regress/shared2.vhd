entity shared2 is
end entity;

architecture test of shared2 is

    type ram_t is array (integer range <>) of bit_vector(7 downto 0);

    shared variable ram : ram_t(1 to 8) := (
        1 => X"11",
        2 => X"22",
        3 => X"33",
        others => X"00" );

begin

    update: process is
    begin
        wait for 1 ns;
        ram(5) := X"aa";
        wait;
    end process;

    check: process is
    begin
        assert ram(1) = X"11";
        assert ram(2) = X"22";
        assert ram(5) = X"00";
        wait for 5 ns;
        assert ram(5) = X"aa";
        wait;
    end process;

end architecture;
