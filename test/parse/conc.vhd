entity ee is end entity;
architecture aa of ee is
    signal x, a, b, c : bit;
    signal foo, bar   : boolean;
    signal y          : integer;
    signal v          : bit_vector(1 to 2);

    procedure pcall(x : in bit; y : in integer);
    procedure xxx;
begin

    x <= a or b;

    postponed x <= '1' when foo
         else '1' when bar
         else '0';

    with y select x <=
        '0' when 6,
        '1' when 5,
        '1' when others;

    pcall(x, y);

    assert y = 5;

    (a, b) <= v;

    xxx;

    b1: block is
        generic ( g1 : integer; g2 : bit := '1' );
        generic map ( g1 => 5 );
        port ( p1 : integer );
        port map ( p1 => y );
    begin
    end block;

end architecture;
