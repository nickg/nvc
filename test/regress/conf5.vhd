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

entity top is
end entity;

architecture test of top is
    component sub_r is
        port (
            x : in real;
            z : out real );
    end component;

    component sub_i is
        port (
            x : in integer;
            z : out integer );
    end component;

    signal xr, zr : real := 0.0;
    signal xi, zi : integer := 0;
begin

    u1: sub_r port map ( xr, zr );

    u2: sub_i port map ( xi, zi );

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

-------------------------------------------------------------------------------

configuration conf5 of top is
    for test
        for u1 : sub_r use entity work.sub
            generic map ( real )
            port map ( x => x, y => x, z => z );
        end for;
        for u2 : sub_i use entity work.sub
            generic map ( integer )
            port map ( x => x, y => x, z => z );
        end for;
    end for;
end configuration;
