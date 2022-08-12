entity sub is
    generic ( width : positive );
    port (
        clk : in bit;
        rst : in bit;
        vec : out integer_vector(1 to width) );
end entity;

architecture test of sub is
begin

    g1: if width = 1 generate

        p: process (clk) is
        begin
            if rst = '1' then
                vec(1) <= 0;
            elsif rising_edge(clk) then
                vec(1) <= vec(1) + 1;
            end if;
        end process;

    else g2: generate

        u1: entity work.sub
            generic map ( width / 2 )
            port map ( clk, rst, vec(1 to width / 2) );

        u2: entity work.sub
            generic map ( width / 2 )
            port map ( clk, rst, vec(width/2 + 1 to width) );
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab34 is
end entity;

architecture test of elab34 is
    signal clk, rst : bit := '1';
    signal vec : integer_vector(1 to 8);
begin

    top: entity work.sub
        generic map ( 8 )
        port map ( clk, rst, vec );

    check: process is
    begin
        wait for 1 ns;
        rst <= '0';
        clk <= '0';
        assert vec = (1 to 8 => 0);
        wait for 1 ns;
        clk <= '1';
        assert vec = (1 to 8 => 0);
        wait for 0 ns;
        assert vec = (1 to 8 => 0);
        wait for 0 ns;
        assert vec = (1 to 8 => 1);
        clk <= '0';
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        assert vec = (1 to 8 => 2);
        wait;
    end process;

end architecture;
