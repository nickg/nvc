package pack is
    generic ( g : integer );
end package;

-------------------------------------------------------------------------------

entity e is
end entity;

architecture test of e is
    package inst is new work.pack generic map ( 4 );
    use inst.all;

    constant c1 : integer := inst.g;    -- OK
    constant c2 : integer := g;         -- OK
    constant c3 : integer := work.pack.g;  -- Error
begin

    b: block is
        signal s : integer;
    begin
    end block;

    b.s <= 3;                           -- Error

end architecture;
