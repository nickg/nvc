entity ram1 is
end entity;

architecture test of ram1 is
    type byte_array_t is array (natural range <>) of bit_vector(7 downto 0);
    signal ram  : byte_array_t(15 downto 0);
    signal addr : integer;
    signal dout : bit_vector(7 downto 0);
    signal din  : bit_vector(7 downto 0);
    signal we   : bit;
begin

    dout_p: dout <= ram(addr);

    wr_p: process (we, din, addr) is
    begin
        if we = '1' then
            ram(addr) <= din;
        end if;
    end process;

end architecture;
