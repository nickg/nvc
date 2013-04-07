entity wait10 is
end entity;

architecture test of wait10 is
    signal s : bit_vector(3 downto 0);
    signal n : integer := 0;
begin

    a: process is
        variable cnt : integer := 0;
    begin
        wait on s(2 downto 1);
        cnt := cnt + 1;
        n <= cnt;
    end process;

    b: process is
    begin
        s <= "0000";
        wait for 1 ns;
        s <= "1001";
        wait for 1 ns;
        s <= "0101";
        wait for 1 ns;
        s <= "0010";
        wait for 1 ns;
        s(1) <= '0';
        s(2) <= '1';
        wait for 1 ns;
        s(2) <= '0';
        wait for 1 ns;
        report integer'image(n);
        assert n = 4;
        wait;
    end process;

end architecture;
