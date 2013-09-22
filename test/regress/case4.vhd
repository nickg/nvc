entity case4 is
end entity;

architecture test of case4 is
    constant c1 : bit_vector(7 downto 0) := X"ab";
    constant c2 : bit_vector(7 downto 0) := X"62";
    signal s    : bit_vector(7 downto 0);
    signal x, y : natural;
begin

    process (s) is
    begin
        case s is
            when c1 =>
                x <= x + 1;
            when c2 | X"50" =>
                y <= y + 1;
            when others =>
                null;
        end case;
    end process;

    process is
    begin
        s <= c1;
        wait for 1 ns;
        s <= c2;
        wait for 1 ns;
        s <= X"63";
        wait for 1 ns;
        s <= X"50";
        wait for 1 ns;
        assert x = 1;
        assert y = 2;
        wait;
    end process;

end architecture;
