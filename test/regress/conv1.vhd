entity conv1 is
end entity;

architecture test of conv1 is
    type my_bit_vector is array (natural range <>) of bit;

    signal x : bit_vector(7 downto 0);
    signal y : my_bit_vector(3 downto 0);
begin

    process is
    begin
        x <= X"ab";
        wait for 1 ns;
        y <= my_bit_vector(x(3 downto 0));
        wait for 1 ns;
        assert y = X"b";
        wait;
    end process;

end architecture;
