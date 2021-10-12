package pack is
    type rec is record
        x : integer;
        y : integer;
        z : boolean;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( r : rec );
    port (
        i : in bit_vector(1 to r.y);
        o : out bit );
end entity;

architecture test of sub is
begin

    g1: if r.z generate
        o <= i(r.x);
    end generate;

    g2: if not r.z generate
        o <= '1';
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity record18 is
end entity;

use work.pack.all;

architecture test of record18 is
    constant r1 : rec := (1, 2, true);
    constant r2 : rec := (0, 0, false);

    signal o1, o2 : bit;
begin

    sub1_i: entity work.sub
        generic map (r1) port map ( "10", o1 );

    sub2_i: entity work.sub
        generic map (r2) port map ( (others => '0'), o2 );

    process is
    begin
        wait for 1 ns;
        assert o1 = '1';
        assert o2 = '1';
        wait;
    end process;

end architecture;
