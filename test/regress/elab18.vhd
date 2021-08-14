entity sub is
    port (
        a    : in bit_vector(1 downto 0);
        b, c : out bit_vector(1 downto 0);
        d    : out bit_vector(1 downto 0) := "00" );
end entity;

architecture test of sub is
begin

    p1: (b(0), c(0)) <= a;
    p2: (b(1), c(1)) <= a;

    p3: d(1) <= '1';

end architecture;

-------------------------------------------------------------------------------

entity elab18 is
end entity;

architecture test of elab18 is
    signal a1, b1, a2, b2 : bit_vector(1 downto 0);
begin

    sub1_i: entity work.sub
        port map (
            a => a1,
            b => b1,
            c => open );

    p4: process is
    begin
        a1 <= "01";
        wait for 1 ns;
        assert b1 = "00";
        wait;
    end process;

    sub2_i: entity work.sub
        port map (
            a => a2,
            b => b2 );

    p5: process is
    begin
        a2 <= "10";
        wait for 1 ns;
        assert b2 = "11";
        wait;
    end process;

end architecture;
