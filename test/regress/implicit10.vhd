entity implicit10 is
end entity;

architecture test of implicit10 is
    signal x : integer;
begin

    check: process is
    begin
        assert not x'stable'active;
        assert not x'quiet'active;
        assert not x'quiet(1 ns)'active;

        wait for 1 ns;

        assert not x'quiet(1 ns)'active;

        wait;
    end process;

end architecture;
