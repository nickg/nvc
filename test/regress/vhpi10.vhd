package counter is
   generic (MAX: bit_vector);
   subtype counter_t is bit_vector(MAX'range);
end;

entity sub is
    generic ( g0 : integer;
              g1 : string );
    port ( p0 : bit_vector );
end entity;

architecture test of sub is
    signal dyn : bit_vector(1 to g1'length);
begin

    sub_b0: block is
        signal s : bit;
    begin
        s <= '1';
    end block;

    ForGen1: for i in 1 to 3 generate
        signal s : integer;
    begin
        s <= i;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity vhpi10 is
    generic (
        g0 : integer := 42;
        g1 : string := "hello" );
end entity;

architecture test of vhpi10 is
    constant c0 : integer := 5;
    constant c1 : real := 1.5;

    component sub is
        generic ( g0 : integer; g1 : string );
        port ( p0 : bit_vector );
    end component;

   package pkg is new work.counter generic map (MAX => 10D"999");
   signal s0: pkg.counter_t;

    function func return string is
    begin
        return "world";
    end function;
begin

    b0: block is
        signal s0 : bit;
    begin
    end block;

    b1: block (true) is
        type t_abc is (a, b, c);
        signal s0 : t_abc;
    begin
    end block;

    i0: component sub
        generic map ( g0 => 100, g1 => func )
        port map ( p0 => "101" );

end architecture;
