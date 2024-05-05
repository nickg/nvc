entity sub is
    generic (
        type t;
        function "+" (a, b : t) return t is <> );
    port (
        x, y : in t;
        z : out t );
end entity;

architecture test of sub is
begin
    z <= x + y;
end architecture;

-------------------------------------------------------------------------------

entity gentype8 is
end entity;

architecture test of gentype8 is
    component sub is
        generic (
            type t;
            function "+" (a, b : t) return t is <> );
        port (
            cx : in t;
            cz : out t );
    end component;

    signal xr, zr : real := 0.0;
    signal xi, zi : integer := 0;

    for all : sub use entity work.sub
        port map ( x => cx, y => cx, z => cz );

begin

    u1: sub generic map ( real ) port map ( xr, zr );

    u2: sub generic map ( integer ) port map ( xi, zi );

    check: process is
    begin
        xr <= 1.0;
        wait for 1 ns;
        assert zr = 2.0;
        xr <= 5.0;
        wait for 1 ns;
        assert zr = 10.0;

        xi <= 8;
        wait for 1 ns;
        assert zi = 16;
        xi <= 100;
        wait for 1 ns;
        assert zi = 200;

        wait;
    end process;

end architecture;
