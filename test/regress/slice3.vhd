entity slice3 is
end entity;

architecture test of slice3 is
    type bvv is array (integer range <>) of bit_vector(7 downto 0);
    signal x : bvv(1 downto 0);
    signal y : bit_vector(3 downto 0);
begin

    process (y) is
    begin
        for i in x'range loop
            x(i)(3 downto 0) <= y;
            x(i)(7 downto 4) <= X"0";
        end loop;
    end process;

    process is
    begin
        wait for 1 ns;
        assert x = ( X"00", X"00" );
        y <= X"f";
        wait for 1 ns;
        assert x = ( X"0f", X"0f" );
        wait;
    end process;

end architecture;
