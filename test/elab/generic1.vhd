entity sub is
    generic (g : integer; m : string; b : boolean);
    port (o : out integer);
end entity;

architecture test of sub is
begin
    p1: o <= g;
    p2: assert b report m;
end architecture;

-------------------------------------------------------------------------------

entity generic1 is
end entity;

architecture test of generic1 is
    type rec is record
        x : integer;
        y : string(1 to 5);
    end record;

    function get_rec return rec is
    begin
        return (x => 1, y => "hello");
    end function;

    constant c : rec := get_rec;
    signal s : integer;
begin

    u: entity work.sub
        generic map ( c.x, c.y, false )
        port map ( s );

end architecture;
