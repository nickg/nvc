entity signal4 is
end entity;

architecture test of signal4 is
    signal s : bit_vector(3 downto 0) := (1 => '1', others => '0');
begin

    process is
        variable v : bit_vector(3 downto 0) := (others => '1');
    begin
        assert s(0) = '0';
        assert s(1) = '1';
        assert v(1) = '1';
        v(2) := s(3);
        assert v(2) = '0';
        s(0) <= v(3);
        assert s(0) = '0';
        wait for 1 ns;
        assert v(3) = '1';
        assert s(0) = v(3);
        assert v = ( '1', '0', '1', '1' );
        s <= v;
        assert s = ( '0', '0', '1', '1' ) report "one";
        wait for 1 ns;
        assert s = ( '1', '0', '1', '1' ) report "two";
        wait;
    end process;

end architecture;
