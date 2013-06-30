entity sub is
    port (
        x : in bit_vector(7 downto 0) );
end entity;

architecture empty of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    signal y : bit_vector(9 downto 1);
begin

    uut: entity work.sub
        port map (
            x => y );

end architecture;
