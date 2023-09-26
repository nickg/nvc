entity sub is
    port ( x : out natural );
end entity;

architecture test of sub is
begin

    b: block is
        port ( x : out natural );
        port map ( x => x );
    begin
        x <= 1;
    end block;

end architecture;

-------------------------------------------------------------------------------

entity block2 is
end entity;

architecture test of block2 is
    signal s : natural;
begin

    u: entity work.sub
        port map ( s );

end architecture;
