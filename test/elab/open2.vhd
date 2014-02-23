entity sub2 is
    generic (
        WIDTH : integer );
    port (
        x : in bit_vector(WIDTH - 1 downto 0);
        y : out bit_vector(WIDTH - 1 downto 0) );
end entity;

architecture test of sub2 is
begin

    gen: for i in 0 to WIDTH - 1 generate
        y(i) <= not x(i);
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity sub1 is
    port (
        x : in bit;
        y : out bit );
end entity;

architecture test of sub1 is
begin

    sub2_i: entity work.sub2
        generic map (
            WIDTH => 1 )
        port map (
            x(0) => x,
            y(0) => y );

end architecture;

-------------------------------------------------------------------------------

entity elab22 is
end entity;

architecture test of elab22 is
    signal a, b : bit;
begin

    sub1_i: entity work.sub1
        port map (
            x => a );

end architecture;
