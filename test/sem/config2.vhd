entity sub is
    generic ( g : integer; g2 : bit );
    port ( i : in bit_vector;
           o : out bit_vector );
end entity;

architecture test of sub is
begin
end architecture;

-------------------------------------------------------------------------------

entity other is
    generic ( gg : integer; g2 : real );
    port ( ii : in bit_vector;
           oo : out bit_vector;
           zz : out integer );
end entity;

architecture test of other is
begin
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    component comp is
        generic ( g : integer; g2 : bit );
        port ( i : in bit_vector;
               o : out bit_vector;
               k : out bit;
               zz : out real );
    end component;

    signal x, y : bit_vector(3 downto 0);
begin

    u1: component comp                  -- Error
        generic map (5, '1')
        port map (x, y);

    u2: component comp                  -- OK
        generic map (5, '1')
        port map (y, x);

end architecture;

-------------------------------------------------------------------------------

configuration conf of top is
    for test
        for u1 : comp
            use entity work.other(test);  -- Error
        end for;
        for u2 : comp
            use entity work.sub(test);
        end for;
    end for;
end configuration;
