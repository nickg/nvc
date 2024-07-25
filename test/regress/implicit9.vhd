entity implicit9 is
end entity;

architecture test of implicit9 is
    signal i1, i2 : natural;
    signal b1, b2 : boolean;
begin

    delayed1: i2 <= i1'delayed(5 ns);

    stable1: b1 <= i1'stable;

    stable2: b2 <= i1'stable(2 ns);

    check: process is
    begin
        i1 <= 4;
        wait for 0 ns;
        --assert i1'stable'active;
        assert b1;
        assert b2;
        wait for 0 ns;
        assert i1'stable'active;
        assert not b1;
        assert not b2;
        wait for 0 ns;
        assert not i1'stable'active;
        assert b1;
        assert not b2;
        wait for 1 ns;
        assert not i1'stable'active;
        assert b1;
        assert not b2;
        wait for 3 ns;
        assert not i1'stable'active;
        assert b1;
        assert i2 = 0;
        wait for 1 ns;
        assert not i1'stable'active;
        assert b1;
        assert i2 = 0;
        wait for 0 ns;
        assert not i1'stable'active;
        assert i2 = 4;
        assert b1;

        i1 <= 20;
        wait for 0 ns;
        --assert i1'stable'active;
        assert not i1'stable;
        wait for 0 ns;
        assert i1'stable'active;

        wait;
    end process;

end architecture;
