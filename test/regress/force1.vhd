entity force1 is
end entity;

architecture test of force1 is
    signal s : natural;
begin

    p1: s <= 1 after 1 ns, 2 after 2 ns, 3 after 3 ns, 4 after 4 ns;

    p2: process is
    begin
        assert s = 0;
        wait for 1 ns;
        assert s = 1;
        assert s'active;
        s <= force 42;
        assert s = 1;
        wait for 0 ns;
        assert s'active;
        assert s'event;
        assert s = 42;
        wait for 0 ns;
        assert not s'active;
        wait for 2 ns;
        assert s = 42;
        s <= release;
        assert s = 42;
        wait for 0 ns;
        assert s'active;
        assert s = 3;
        wait for 0 ns;
        assert not s'active;
        wait;
    end process;

end architecture;
