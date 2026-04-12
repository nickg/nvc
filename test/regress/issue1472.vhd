entity signal_checker_v2 is
    port(
        value : in bit_vector
    );
end entity signal_checker_v2;

architecture tb of signal_checker_v2 is
begin
    process is
    begin
        report "Value is 0x" & to_hstring(value);
        assert value = x"ABCDEF01";
        assert value'left = 2;
        assert value'right = 33;
        assert value'ascending;
        wait;
    end process;
end architecture tb;

entity issue1472 is
end entity issue1472;

architecture tb of issue1472 is
    type unsigned is array (integer range <>) of bit;

    signal Data : unsigned(2 to 33) := x"ABCDEF01";
begin

    i_test : entity work.signal_checker_v2
    port map(
        value(Data'range) => bit_vector(Data)
    );

end architecture tb;
