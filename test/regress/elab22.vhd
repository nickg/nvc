entity sub is
    port (
        i : in  bit_vector(0 to 7);
        o : out bit_vector(0 to 7) );
end entity;

architecture test of sub is
begin

    o <= not i after 1 ns;

end architecture;

-------------------------------------------------------------------------------

entity elab22 is
end entity;

architecture test of elab22 is
    signal a : bit_vector(0 to 1);
    signal b : bit_vector(0 to 5);
    signal c : bit_vector(2 to 5);
    signal d : bit_vector(0 to 3);
begin

    sub_i: entity work.sub
        port map (
            i(0 to 1) => a,
            i(2 to 7) => b,
            o(0 to 3) => c,
            o(4 to 7) => d );

    process is
    begin
        assert c = "0000";
        assert d = "0000";
        wait for 2 ns;
        assert c = "1111";
        assert d = "1111";
        a <= "11";
        wait for 2 ns;
        assert c = "0011";
        assert d = "1111";
        b <= "011110";
        wait for 2 ns;
        assert c = "0010";
        assert d = "0001";
        wait;
    end process;

end architecture;
