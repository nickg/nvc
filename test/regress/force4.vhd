entity force4 is
end entity;

architecture test of force4 is
    signal s, cnt : natural;
begin

    update: process (s) is
    begin
        cnt <= cnt + 1;
    end process;

    stim: process is
    begin
        wait for 1 ns;
        assert cnt = 1;
        s <= force 1;
        wait for 0 ns;
        assert cnt = 1;
        wait for 0 ns;
        assert cnt = 2;
        s <= force 2;
        s <= force 1;
        wait for 0 ns;
        assert cnt = 2;
        wait for 0 ns;
        assert cnt = 3;                 -- This is ambiguous in the LRM
        wait;
    end process;

end architecture;
