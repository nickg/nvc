entity sanity2 is
    port ( p : out bit_vector );
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

    p2: p(1) <= '1';                    -- { P(1) }
    p3: p(2 to p'length) <= (others => '0');

end architecture;
