entity attr14 is
end entity;

architecture test of attr14 is
    type rec is record
        x : bit_vector(7 downto 0);
    end record;

    signal s : rec;
    signal t : bit_vector(3 downto 0);
begin

    p1: process is
    begin
        wait for 1 ns;
        s.x(4) <= '1';
        wait for 1 ns;
        s.x(4) <= '1';
        wait;
    end process;

    p2: process is
    begin
        wait on s.x;
        assert now = 1 ns;
        assert s.x'event;
        assert s.x'last_event = 0 ns;
        assert s.x'last_active = 0 ns;
        assert s.x(4) = '1';

        wait on s.x;
        assert s.x'active;
        assert not s.x'event;
        assert s.x'last_event = 1 ns;
        assert s.x'last_active = 0 ns;
        assert s.x(4) = '1';

        wait;
    end process;

end architecture;
