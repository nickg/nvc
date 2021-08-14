entity arrayref3 is
end entity;

architecture test of arrayref3 is
    type ram_t is array (integer range <>) of bit_vector(3 downto 0);
    signal ram  : ram_t(0 to 3);        -- 0..15
    signal addr : integer;              -- 16
begin

    process (addr) is
    begin
        ram(addr)(1 downto 0) <= "11";
        ram(addr)(3 downto 2) <= "00";
    end process;

end architecture;
