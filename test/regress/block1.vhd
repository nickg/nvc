entity block1 is
end entity;

architecture test of block1 is
    signal u, v, w: integer;
begin

    process is
    begin
        u <= 1;
        wait for 1 ns;
        u <= 2;
        wait;
    end process;

    a: block is
        signal x : integer;
    begin
        x <= u + 2;
        v <= x;
    end block;

    b: block is
        signal x : integer;
    begin
        x <= v + 6;
        w <= x;
    end block;

    process is
    begin
        wait for 1 ns;
        assert w = 9;
        wait for 1 ns;
        assert w = 10;
        wait;
    end process;
    
end architecture;
