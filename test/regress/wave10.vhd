entity sub is
    generic ( g : in integer );
    port ( x : in bit_vector(1 to g) );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity wave10 is
end entity;

architecture test of wave10 is
    component comp is
        generic ( g : in integer );
        port ( x : in bit_vector(1 to g) );
    end component;

    function get_g return integer is
    begin
        return 5;
    end function;

    for u1 : comp use entity work.sub;
    for u2 : comp use open;

    signal s : bit_vector(1 to get_g);
begin

    u1: component comp
        generic map (g => get_g)
        port map (s);

    u2: component comp
        generic map (g => get_g)
        port map (s);

    s <= "01000" after 1 ns, "11100" after 2 ns;

end architecture;
