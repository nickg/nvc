package pack is

    type int_vec is array (natural range <>) of integer;

    type params_t is record
        width : natural;
        scale : natural;
    end record;

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic (
        depth : natural;
        params : params_t );
    port ( vec : in int_vec(1 to 2 ** (depth * params.scale)) );
end entity;

architecture test of sub is
begin

    g1: if depth > 0 generate
        constant mid : integer := 2 ** ((depth - 1) * params.scale);
        constant a_low : integer := 1;
        constant a_high : integer := mid;
        constant b_low : integer := mid + 1;
        constant b_high : integer := 2 ** (depth * params.scale);
    begin
        a: entity work.sub
            generic map ( depth - 1, params )
            port map ( vec(a_low to a_high) );
        b: entity work.sub
            generic map ( depth - 1, params )
            port map ( vec(b_low to b_high) );
    end generate;

    g2: if depth = 0 generate
    begin
        assert vec = (1 => 42);
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab38 is
end entity;

use work.pack.all;

architecture test of elab38 is
    constant params : params_t := ( width => 5, scale => 1 );
    signal vec : int_vec(1 to 4) := (42, 42, 42, 42);
begin

    u: entity work.sub
        generic map ( 2, params )
        port map ( vec );

end architecture;
