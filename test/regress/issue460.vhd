entity issue460 is
end entity;

architecture test of issue460 is
    signal a, b : bit;
    signal x : natural;
begin

    p1: process (a, b) is
    begin
        case bit_vector'(a & b) is
            when "10" =>
                x <= 1;
            when "01" =>
                x <= 2;
            when others =>
                x <= 3;
        end case;
    end process;

    check: process is
    begin
        wait for 0 ns;
        assert x = 3;
        a <= '1';
        wait for 1 ns;
        assert x = 1;
        b <= '1';
        wait for 1 ns;
        assert x = 3;
        a <= '0';
        wait for 1 ns;
        assert x = 2;
        wait;
    end process;

end architecture;
