entity bounds8 is
end entity;

architecture test of bounds8 is
    signal a : bit_vector(3 downto 0);
    signal b : bit_vector(7 downto 0);
begin

    b <= X"10";

    process is
    begin
        a <= b;
        wait;
    end process;

end architecture;
