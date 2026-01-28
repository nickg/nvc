entity issue1396 is
end entity;

architecture FULL of issue1396 is

    type  n_array_t is array (natural range <>) of natural;

    constant SIZE    : natural := 4;
    constant OFFSETS : n_array_t(SIZE+1-1 downto 0) := (
        0 => 0,
        1 => 2,
        2 => 6,
        3 => 10,
        4 => 11
    );
    constant NWIDTH   : natural := 11;

    subtype F_C is natural range OFFSETS(1)-1 downto OFFSETS(0);
    subtype F_I is natural range OFFSETS(3)-1 downto OFFSETS(2);

    signal s_data : bit_vector(NWIDTH-1 downto 0) := (others => '0');

begin

--    s_data_p: process (all)
--    begin
        s_data(F_C)  <= "11";
        s_data(F_I) <= (others => '1');
--    end process;

-- This is working fine
--        s_data(2 downto 1)  <= "11";
--        s_data(7 downto 4) <= (others => '1');

end architecture;
