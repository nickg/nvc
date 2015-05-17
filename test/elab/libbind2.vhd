entity sub is
    port ( x : out integer );
end entity;

architecture test of sub is
begin
    x <= 4;
end architecture;

-------------------------------------------------------------------------------

library other;

entity top is
end entity;

architecture test of top is

    component sub is
        port ( x : out integer );
    end component;

    signal x : integer;

begin

    sub_i: component sub
        port map ( x => x );

end architecture;
