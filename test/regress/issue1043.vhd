entity issue1043 is
end entity;

architecture test of issue1043 is
    type t_rec is record
        x : integer;
        y : real;
    end record;

    type t_array is array (natural range <>) of t_rec;

    signal s : t_rec := (6, 42.0);
    signal t : t_array(1 to 3) := (others => (0, 0.0));
begin

    process is
    begin
        assert s'last_value = (6, 42.0);
        assert t'last_value(1) = (0, 0.0);
        s <= (4, 22.0);
        wait for 1 ns;
        assert s'last_value.x = 6;
        t(3) <= (8, 55.0);
        wait for 1 ns;
        assert t'last_value = (1 to 3 => (0, 0.0));
        wait;
    end process;

end architecture;
