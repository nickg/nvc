entity sub is
    port ( p : in bit_vector(7 downto 0) );
end entity;

architecture test of sub is
    alias a1 : bit_vector(3 downto 0) is p(7 downto 4);
    alias a2 : bit_vector(3 downto 0) is p(3 downto 0);
begin

    process is
    begin
        wait for 2 ns;
        assert a1 = "1111";
        assert a2 = "0000";

        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity alias11 is
end entity;

architecture test of alias11 is

    signal s : bit_vector(7 downto 0);
    alias a1 : bit_vector(3 downto 0) is s(7 downto 4);
    alias a2 : bit_vector(3 downto 0) is s(3 downto 0);

begin

    sub_i: entity work.sub port map (s);

    process is
    begin
        s <= "10100011";
        wait for 1 ns;
        assert a1 = "1010";
        assert a2 = "0011";

        a1 <= "1111";
        a2 <= "0000";
        wait for 1 ns;
        assert s = "11110000";

        wait;
    end process;

end architecture;
