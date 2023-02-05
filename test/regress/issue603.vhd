entity issue603 is
end entity;

architecture test of issue603 is
    signal x : bit_vector(1 to 3);
begin

    p1: process is
    begin
        assert x(1)'delayed = '0';
        assert x(2)'delayed = '0';
        x <= "100";
        wait for 0 ns;
        assert x(1)'delayed = '0';
        assert x(2)'delayed = '0';
        wait for 0 ns;
        assert x(1)'delayed = '1';
        assert x(2)'delayed = '0';
        x(2) <= '1';
        wait for 0 ns;
        x(2) <= '0';
        wait for 0 ns;
        assert x(2)'delayed = '1';
        wait for 0 ns;
        assert x(2)'delayed = '0';
        wait;
    end process;

    g1: for i in 1 to 3 generate
    begin
        p2: process is
        begin
            assert x(i)'delayed = '0';
            wait;
        end process;
    end generate;

end architecture;
