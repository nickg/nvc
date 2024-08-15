entity sub is
    port ( x : in bit;
           y : out bit );
end entity;

architecture test of sub is
begin
    y <= not x after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity issue941 is
end entity;

architecture test of issue941 is
    component comp is
        port ( x : in bit;
               y : out bit );
    end component;

    for all : comp use entity work.sub;

    signal a, b : bit;
begin

    a <= '1' after 2 ns, '0' after 5 ns;

    u1: component comp
        port map ( a, b );

end architecture;
