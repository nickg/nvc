entity operator4 is
end entity;

architecture test of operator4 is

    type byte_vec is array (integer range <>) of bit_vector(7 downto 0);

begin

    process is
        variable v : byte_vec(1 to 3);
    begin
        v := ( X"01", X"02", X"03" );
        assert v = ( X"01", X"02", X"03" );
        assert v /= ( X"01", X"02", X"05" );
        assert v /= ( X"01", X"02", X"03", X"04" );
        assert v /= ( X"01", X"02" );
        wait;
    end process;

end architecture;
