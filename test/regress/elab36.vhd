entity sub is
    generic ( id : natural );
    port ( o : out integer );
end entity;

architecture test of sub is
begin
    g1: if id = 1 generate
        o <= 1;
    end generate;

    g2: if id = 2 generate
        o <= 2;
    end generate;

    g3: if id = 3 generate
        o <= 3;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab36 is
end entity;

architecture test of elab36 is
    shared variable counter : natural := 0;

    impure function get_id return natural is
    begin
        counter := counter + 1;
        return counter;
    end function;

    signal s : integer;
    constant id : natural := get_id;
begin

    u: entity work.sub
        generic map ( id )
        port map ( s );

    p1: process is
    begin
        wait for 1 ns;
        assert s = 1;
        wait;
    end process;

end architecture;
