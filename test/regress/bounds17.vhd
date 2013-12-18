entity bounds17 is
end entity;

architecture test of bounds17 is

    function get_bits(n : natural) return bit_vector is
    begin
        return (n - 1 downto 0 => '0');
    end function;

begin

    process is
        variable x : bit_vector(7 downto 0) := get_bits(12);
    begin
        report "should not print this";
        wait;
    end process;

end architecture;
