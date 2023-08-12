entity sanity2 is
end entity;

architecture test of sanity2 is
    signal x, y : bit_vector(7 downto 0);
begin

    p0: process is                      -- { X(3 downto 0) }
    begin
        x(3 downto 0) <= X"0";
        x(3 downto 0) <= X"1";
        wait;
    end process;

    p1: process is                      -- { Y }
    begin
        y <= X"00";
        y(3 downto 0) <= X"f";
        y(4) <= '1';
        wait;
    end process;

end architecture;
