entity logical1 is
end entity;

architecture test of logical1 is
    signal x : bit;
begin

    process is
    begin
        x <= '0';
        wait for 1 ns;
        assert (x and '0') = '0';
        assert (x and '1') = '0';
        assert (x or '0') = '0';
        assert (x or '1') = '1';
        assert (x xor '0') = '0';
        assert (x xor '1') = '1';
        assert (x xnor '0') = '1';
        assert (x xnor '1') = '0';
        assert (x nand '0') = '1';
        assert (x nand '1') = '1';
        assert (x nor '0') = '1';
        assert (x nor '1') = '0';

        x <= '1';
        wait for 1 ns;
        assert (x and '0') = '0';
        assert (x and '1') = '1';
        assert (x or '0') = '1';
        assert (x or '1') = '1';
        assert (x xor '0') = '1';
        assert (x xor '1') = '0';
        assert (x xnor '0') = '0';
        assert (x xnor '1') = '1';
        assert (x nand '0') = '1';
        assert (x nand '1') = '0';
        assert (x nor '0') = '0';
        assert (x nor '1') = '0';

        wait;
    end process;

end architecture;
