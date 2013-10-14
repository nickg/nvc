entity sub is
    port (
        i : in  bit_vector(7 downto 0);
        o : out bit_vector(7 downto 0) );
end entity;

architecture test of sub is
begin

    o <= not i after 1 ns;

end architecture;

-------------------------------------------------------------------------------

entity elab14 is
end entity;

architecture test of elab14 is
    signal a : bit_vector(1 downto 0);
    signal b : bit_vector(5 downto 0);
    signal c : bit_vector(5 downto 2);
    signal d : bit_vector(3 downto 0);
begin

    sub_i: entity work.sub
        port map (
            i(1 downto 0) => a,
            i(7 downto 2) => b,
            o(3 downto 0) => c,
            o(7 downto 4) => d );

    process is
    begin
        assert c = "0000";
        assert d = "0000";
        wait for 2 ns;
        assert c = "1111";
        assert d = "1111";
        a <= "11";
        wait for 2 ns;
        assert c = "1100";
        assert d = "1111";
        b <= "011110";
        wait for 2 ns;
        assert c = "0100";
        assert d = "1000";
        wait;
    end process;

end architecture;
