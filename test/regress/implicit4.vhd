entity implicit4 is
end entity;

architecture test of implicit4 is
    signal x : integer;
begin

    p1: process is
    begin
        assert x'last_active = time'high;
        assert x'stable;
        assert x'quiet;
        assert x'stable(1 hr);
        assert x'quiet(1 hr);
        x <= 4;
        wait for 0 ns;
        assert not x'stable;
        assert not x'quiet;
        wait for 0 ns;
        assert x'stable;
        assert x'quiet;
        assert not x'stable(1 ns);
        assert not x'quiet(1 ns);
        wait for 1 ns;
        assert x'stable(1 ns);
        assert x'quiet(1 ns);
        x <= 4;
        wait for 0 ns;
        assert x'stable;
        assert not x'quiet;
        wait;
    end process;

end architecture;
