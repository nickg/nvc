entity wait25 is
end entity;

architecture test of wait25 is
    signal a, b, c, d : natural;
begin

    p1: process (a, b) is
    begin
        report "P1: a=" & integer'image(a) & " b=" & integer'image(b);
    end process;

    p2: process is
    begin
        wait on c, d for 3 ns;
        report "P2: c=" & integer'image(c) & " d=" & integer'image(d);
        assert now = 3 ns;
        assert c = 0;
        assert d = 0;
        wait on c, d;
        report "P2: c=" & integer'image(c) & " d=" & integer'image(d);
        assert now = 5 ns;
        assert c = 1;
        assert d = 1;
        wait on c, d for 1 ns;
        assert now = 6 ns;
        assert c = 2;
        assert d = 1;
        wait for 1 ns;
        assert now = 7 ns;
        wait;
    end process;

    stim: process is
    begin
        wait for 1 ns;
        a <= 1;
        b <= 1;
        wait for 4 ns;
        c <= 1;
        d <= 1;
        c <= transport 2 after 1 ns;
        wait;
    end process;

end architecture;
