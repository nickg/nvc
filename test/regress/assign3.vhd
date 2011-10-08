entity assign3 is
end entity;

architecture test of assign3 is
begin

    process is
        variable x : bit_vector(7 downto 0);
        variable y : bit_vector(7 downto 0) := (
            0 => '1', 2 => '1', 4 => '1', 6 => '1',
            others => '0' );
    begin
        assert x(0) = '0';
        x := (others => '1');
        assert x(0) = '1';
        assert x(5) = '1';
        x := y;
        assert x(0) = '1';
        assert x(5) = '0';
        assert x(6) = '1';
        assert x(7) = '0';
        assert x = y;
        y(4) := '0';
        assert x /= y;
        wait;
    end process;

end architecture;
