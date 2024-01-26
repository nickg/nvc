entity signal35 is
end entity;

architecture test of signal35 is
    signal s : integer := 1000;
begin
    s <= 1 after 1 ns, 5 after 2 ns;

    b: block is
        port ( p : in integer := 0);
        port map ( p => inertial s );
    begin

        check: process is
        begin
            assert p = 0;
            wait for 1 ns;
            assert p = 1000;
            wait for 0 ns;
            assert p = 1;
            wait for 1 ns;
            assert p = 1;
            wait for 0 ns;
            assert p = 5;
            wait;
        end process;

    end block;

end architecture;
