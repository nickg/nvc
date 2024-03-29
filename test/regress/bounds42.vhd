entity bounds42 is
end entity;

architecture test of bounds42 is
    constant c1 : bit_vector(1 to 3) := "111";
    constant c2 : bit_vector(1 to 8) := X"00";
begin

    p1: process is
        procedure test (x : bit_vector) is
        begin
            report to_string( bit_vector'(x'range => (3 downto 0 => '0', 7 downto 4 => '1')));
        end procedure;
    begin
        test(c2);                       -- OK
        test(c1);                       -- Error
        wait;
    end process;

end architecture;
