entity UC is
    port(
        an_input:   in  bit_vector;
        an_output:  out bit_vector
    );
end entity;

architecture test of UC is
begin
    an_output <= an_input;
end architecture;

-------------------------------------------------------------------------------

entity bounds10 is
end entity;

architecture test of bounds10 is
    signal an_input:    bit_vector(  0 downto 0);
    signal an_output:   bit_vector(100 downto 0);
begin
UC:
    entity work.UC
        port map
        (
            an_input    => an_input,
            an_output   => an_output
        );
TEST:
    an_input <= "0", "1" after 1 ns;

end architecture;
