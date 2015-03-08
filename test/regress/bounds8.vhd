entity bounds8 is
end entity;

architecture test of bounds8 is
    signal a : bit_vector(3 downto 0);
    signal b : bit_vector(7 downto 0);
begin

    b <= X"10";

    process is
        variable n : integer;
    begin
        n := 7;
        wait for 1 ns;
        a <= b(n downto 0);
        wait;
    end process;

end architecture;
