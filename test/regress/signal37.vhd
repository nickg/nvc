entity signal37 is
end entity;

architecture test of signal37 is
    type t_rec is record
        f : bit;
    end record;

    type t_array is array (1 to 2) of t_rec;

    signal s : t_array;
begin

    check: process is
    begin
        assert not s'event;
        s <= (others => (f => '1'));
        wait for 0 ns;
        assert s'event;
        wait for 0 ns;
        assert not s'event;
        s(1).f <= '0' after 1 ns;
        wait for 1 ns;
        assert s'event;
        wait for 0 ns;
        assert not s'event;

        wait;
    end process;

end architecture;
