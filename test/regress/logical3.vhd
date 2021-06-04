entity logical3 is
end entity;

architecture test of logical3 is
    signal v : bit_vector(1 to 3);
begin

    process is
    begin
        v <= "110";
        wait for 1 ns;
        assert and v = '0';
        assert nand v = '1';
        assert or v = '1';
        assert nor v = '0';
        assert xor v = '0';
        assert xnor v = '1';

        v <= "111";
        wait for 1 ns;
        assert and v = '1';
        assert nand v = '0';
        assert or v = '1';
        assert nor v = '0';
        assert xor v = '1';
        assert xnor v = '0';

        wait;
    end process;

end architecture;
