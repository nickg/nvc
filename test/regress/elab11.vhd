entity sub is
    port (
        x, y : in bit_vector(3 downto 0);
        z    : out bit_vector(7 downto 0) );
end entity;

architecture test of sub is
begin

    z <= x & y;

end architecture;

-------------------------------------------------------------------------------

entity elab11 is
end entity;

architecture test of elab11 is
    signal a, b : bit_vector(7 downto 0);
begin

    sub_i: entity work.sub
        port map (
            x => a(7 downto 4),
            y => a(5 downto 2),
            z => b );

    process is
    begin
        a <= "11110000";
        wait for 1 ns;
        assert b = "11111100";
        a <= "10110011";
        wait for 1 ns;
        assert b = "10111100";
        wait;
    end process;

end architecture;
