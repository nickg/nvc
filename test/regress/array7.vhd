entity array7 is
end entity;

architecture test of array7 is
    type t_bv_array is array (natural range <>) of bit_vector;
    subtype t_byte_array  is t_bv_array(open)(7 downto 0);

    signal s : t_byte_array(1 to 3);
begin

    p1: process is
    begin
        assert s(1) = X"00";
        s(2) <= X"ab";
        wait for 1 ns;
        assert s = (X"00", X"ab", X"00");
        s(1 to 2) <= (X"01", X"02");
        wait for 1 ns;
        assert s = (X"01", X"02", X"00");
        wait;
    end process;

end architecture;
