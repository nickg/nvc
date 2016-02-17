entity sub is
    port (
        x : in bit_vector(3 downto 0);
        y : out bit );
end entity;

architecture test of sub is
begin
    y <= x(0) xor x(1) xor x(2) xor x(3);
end architecture;

-------------------------------------------------------------------------------

entity issue251 is
end entity;

architecture test of issue251 is
    signal a : bit_vector(3 downto 0);
    signal b : bit_vector(1 downto 0);
begin

    sub1_i: entity work.sub
        port map (
            x(-1) => a(0),               -- Error
            x(3 downto 0) => a,
            y => open );

    sub2_i: entity work.sub
        port map (
            x(3) => a(-1),               -- Error
            x(2 downto 0) => a,
            y => open );

end architecture;
