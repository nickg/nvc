entity conv13 is
end entity;

architecture test of conv13 is
    signal s : integer := 0;
begin

    b1: block is
        port ( p : inout real := 0.0 );
        port map ( integer(p) => real(s) );
    begin
        inner: process is
        begin
            p <= 1.0;
            wait for 0 ns;
            assert p = 1.0;
            p <= 1.5;
            wait for 0 ns;
            assert p = 2.0;
            wait;
        end process;
    end block;

    outer: process is
    begin
        wait for 0 ns;
        assert s = 1;
        wait for 0 ns;
        assert s = 2;
        wait;
    end process;

end architecture;
