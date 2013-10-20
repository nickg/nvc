entity signal12 is
end entity;

architecture test of signal12 is
    type byte_vec is array (integer range <>) of bit_vector(7 downto 0);
    signal a : bit_vector(7 downto 0);
    signal b : byte_vec(1 to 3);
begin

    assign: b <= (others => a);

    process is
    begin
        a <= X"01";
        wait for 1 ns;
        assert b(1) = X"01";
        assert b(2) = X"01";
        assert b(3) = X"01";
        a <= X"f0";
        wait for 1 ns;
        assert b(1) = X"f0";
        assert b(2) = X"f0";
        assert b(3) = X"f0";
        wait;
    end process;

end architecture;
