entity sub2 is
    port (
        x0, x1 : in  bit;
        y0, y1 : out bit );
end entity;

architecture test of sub2 is
begin
    y0 <= x0 after 0 ns;
    y1 <= not x1 after 0 ns;
end architecture;

-------------------------------------------------------------------------------

entity sub is
    port (
        x : in  bit_vector(1 downto 0);
        y : out bit_vector(1 downto 0) );
end entity;

architecture test1 of sub is
begin

    y(0) <= x(0) after 0 ns;
    y(1) <= not x(1) after 0 ns;

end architecture;

architecture test2 of sub is
begin

    sub2_i: entity work.sub2
        port map (
            x0 => x(0),
            x1 => x(1),
            y0 => y(0),
            y1 => y(1) );

end architecture;

-------------------------------------------------------------------------------

entity elab24 is
end entity;

architecture test of elab24 is
begin

    test_a: block is
        signal x0, x1, y0, y1 : bit;
    begin

        sub_i: entity work.sub(test1)
            port map (
                x(0) => x0,
                x(1) => x1,
                y(0) => y0,
                y(1) => y1 );

        process is
        begin
            x0 <= '1';
            wait for 2 ns;
            assert y0 = '1';

            wait;
        end process;

    end block;

    test_b: block is
        signal x0, x1, y0, y1 : bit;
    begin

        sub_i: entity work.sub(test2)
            port map (
                x(0) => x0,
                x(1) => x1,
                y(0) => y0,
                y(1) => y1 );

        process is
        begin
            x0 <= '1';
            wait for 2 ns;
            assert y0 = '1';

            wait;
        end process;

    end block;

end architecture;
