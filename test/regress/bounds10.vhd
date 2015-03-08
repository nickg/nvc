entity bounds10 is
end entity;

architecture test of bounds10 is
begin

    process is
        variable n : integer;
        variable a : bit_vector(3 downto 0);
        variable b : bit_vector(7 downto 0);
    begin
        n := 7;
        wait for 1 ns;
        a := b(n downto 0);
        wait;
    end process;

end architecture;
