package pack is

    type params_t is record
        width : natural;
    end record;

end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic (
        depth : natural;
        params : params_t );
    port ( vec : out bit_vector(1 to params.width) );
end entity;

architecture test of sub is
    constant mid : integer := params.width / 2;
begin

    g1: if depth > 0 generate
        u1: entity work.sub
            generic map ( depth - 1, params )
            port map ( vec(1 to mid) );

        u2: entity work.sub
            generic map ( depth - 1, params )
            port map ( vec(mid + 1 to params.width) );
    end generate;

    g2: if depth = 0 generate
        signal s : bit_vector(1 to params.width);
    begin
        p1: process is
        begin
            assert s'length = 5;
            s <= (others => '1');
            wait;
        end process;

        p2: vec <= s;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab40 is
end entity;

use work.pack.all;

architecture test of elab40 is
    constant params : params_t := ( width => 5 );
    signal vec : bit_vector(1 to 5);
begin

    u: entity work.sub
        generic map ( 2, params )
        port map ( vec );

end architecture;
