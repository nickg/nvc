entity sub is
    port ( addr : in natural;
           data : out integer );
end entity;

architecture test of sub is
    -- This must be at the same address in both instances
    constant mem : integer_vector := (1, 6, 743, 42, 9, 7);
begin
    data <= mem(addr);
end architecture;

-------------------------------------------------------------------------------

entity elab41 is
end entity;

architecture test of elab41 is
    signal addr : natural;
    signal data1, data2 : integer;
begin

    u1: entity work.sub
        port map ( addr, data1 );

    u2: entity work.sub
        port map ( inertial addr, data2 );  -- Creates internal signal

    check: process is
    begin
        wait for 0 ns;
        assert data1 = 1;
        assert data2 = 1;
        addr <= 3;
        wait for 0 ns;
        assert data1 = 1;
        assert data2 = 1;
        addr <= 4;
        wait for 0 ns;
        assert data1 = 42;
        assert data2 = 1;
        wait for 0 ns;
        assert data1 = 9;
        assert data2 = 42;
        wait;
    end process;

end architecture;
