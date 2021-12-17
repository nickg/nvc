entity shift1 is
end entity;

architecture test of shift1 is
begin

    process is
        variable b : bit_vector(3 downto 0);
        variable c : bit_vector(0 to 3);
        variable d : bit_vector(0 to -1);
    begin
        b := "1011";
        c := "1011";

        wait for 1 ns;

        assert (b sll 1) = "0110";
        assert (c sll 1) = "0110";
        assert (b srl 1) = "0101";
        assert (c srl 1) = "0101";
        assert (b sla 1) = "0111";
        assert (c sla 1) = "0111";
        assert (b sra 1) = "1101";
        assert (c sra 1) = "1101";
        assert (b rol 2) = "1110";
        assert (c rol 2) = "1110";
        assert (b ror 1) = "1101";
        assert (c ror 1) = "1101";

        assert (b srl -1) = "0110";
        assert (c srl -1) = "0110";
        assert (b sll -1) = "0101";
        assert (c sll -1) = "0101";
        assert (b sra -1) = "0111";
        assert (c sra -1) = "0111";
        assert (b sla -1) = "1101";
        assert (c sla -1) = "1101";
        assert (b ror -2) = "1110";
        assert (c ror -2) = "1110";
        assert (b rol -1) = "1101";
        assert (c rol -1) = "1101";
        assert (c rol -5) = "1101";

        assert (d sll 1) = d;           -- Null array

        b := "0001";
        wait for 1 ns;

        assert (b sll -4) = "0000";

        wait;
    end process;

end architecture;
