entity issue1332 is
end entity;

architecture arch of issue1332 is
    constant c      : bit_vector(1 downto 0) := "00";
    signal   vec0   : bit_vector(c'range); -- Expected range: (1 downto 0)
    signal   vec1   : bit_vector(1 downto 0);
    signal   s0, s1 : bit := '0';
begin
    vec0 <= (1 => s1, 0 => s0);
    vec1 <= (1 => s1, 0 => s0);

    test : process is
    begin
        wait for 10 ns;
        assert vec0 = "00";
        assert vec1 = "00";

        s1 <= '1';
        s0 <= '0';
        wait for 10 ns;
        assert vec0 = "10";
        assert vec1 = "10";

        wait;
    end process;

    extra: block is
        signal vec2 : bit_vector(vec0'range);
        signal vec3 : bit_vector(vec0'reverse_range);
    begin
        vec2 <= (1 => s1, 0 => s0);
        vec3 <= (1 => s1, 0 => s0);

        process is
        begin
            wait for 20 ns;

            assert vec2 = "10";
            assert vec3 = "01";

            wait;
        end process;

    end block;
end architecture;
