entity sub is
    generic ( def : integer;
              g : integer := def );
    port ( x : out integer );
end entity;

architecture test of sub is
begin
    x <= g;
end architecture;

-------------------------------------------------------------------------------

entity elab30 is
end entity;

architecture test of elab30 is
    signal x : integer;
begin

    u: entity work.sub
        generic map ( def => 42 )
        port map ( x );

    p1: process is
    begin
        wait for 1 ns;
        assert x = 42;
        wait;
    end process;

end architecture;
