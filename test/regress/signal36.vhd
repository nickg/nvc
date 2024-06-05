entity signal36 is
end entity;

architecture test of signal36 is
    type t_rec is record
        x, y : natural;
    end record;

    signal s : t_rec := (1000, 1000);
begin
    s <= (3, 4) after 1 ns, (5, 6) after 2 ns;

    b: block is
        port ( p : in t_rec := (42, 42) );
        port map ( p => inertial s );
    begin

        check: process is
        begin
            assert p = (42, 42);
            wait for 1 ns;
            assert p = (1000, 1000);
            wait for 0 ns;
            assert p = (3, 4);
            wait for 1 ns;
            assert p = (3, 4);
            wait for 0 ns;
            assert p = (5, 6);
            wait;
        end process;

    end block;

end architecture;
