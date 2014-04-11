entity record11 is
end entity;

architecture test of record11 is
    type rec is record
        x, y : bit;
    end record;

    signal r    : rec;
    signal a, b : bit;
begin

    process is
    begin
        r <= ( '1', '0' );
        wait for 0 ns;
        assert r.x'event;
        assert not r.y'event;
        assert r.y'active;
        assert r'event;
        wait for 1 ns;
        assert a'event;
        assert not b'event;
        assert b'active;
        assert a = '1';
        assert b = '0';
        r.y <= '1';
        wait for 1 ns;
        assert b = '1';
        wait;
    end process;

    update_a: a <= r.x after 1 ns;
    update_b: b <= r.y after 1 ns;

end architecture;
