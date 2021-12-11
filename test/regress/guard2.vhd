entity guard2 is
end entity;

architecture test of guard2 is

    type int_vec is array (natural range <>) of integer;

    function resolved (x : int_vec) return integer is
    begin
        return x'length;
    end function;

    subtype rint is resolved integer;

    signal vbus : rint bus;
    signal vreg : rint register;
begin

    check: process is
    begin
        assert vbus = 1;
        assert vbus'driving;
        vbus <= 100;
        wait for 1 ns;
        assert vbus = 1;
        vbus <= null;
        wait for 1 ns;
        assert vbus = 0;
        assert not vbus'driving;

        assert vreg = 1;
        assert vreg'driving;
        vreg <= 123;
        wait for 1 ns;
        assert vreg = 1;
        vreg <= null;
        wait for 1 ns;
        assert vreg = 1;
        assert not vreg'driving;

        wait;
    end process;

end architecture;
