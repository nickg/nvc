entity bounds42 is
end entity;

architecture test of bounds42 is
    constant c1 : bit_vector(1 to 3) := "111";
    constant c2 : bit_vector(1 to 8) := X"00";
begin

    p1: process is
    begin
        -- OK
        report to_string( bit_vector'(c2'range => (3 downto 0 => '0', 7 downto 4 => '1')));

        -- Error
        report to_string( bit_vector'(c1'range => (3 downto 0 => '0', 7 downto 4 => '1')));

        wait;
    end process;

end architecture;
