entity sub2 is
    port (
        x : in bit;
        y : out bit );
end entity;

architecture test of sub2 is
begin
    y <= x after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity sub1 is
    port (
        x : in bit_vector(7 downto 0);
        y : out bit_vector(7 downto 0) );
end entity;

architecture test of sub1 is
begin

    sub_g: for i in x'range generate

        sub2_i: entity work.sub2
            port map ( x(i), y(i) );

    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab19 is
end entity;

architecture test of elab19 is
    signal o : bit_vector(7 downto 0);
begin

    sub1_i: entity work.sub1
        port map ( X"ab", o );

    process is
    begin
        wait for 2 ns;
        assert o = X"ab";
        wait;
    end process;

end architecture;
