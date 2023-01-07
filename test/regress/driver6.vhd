entity driver6 is
end entity;

architecture test of driver6 is
    type int_vec is array (natural range <>) of integer;

    function resolved (v : int_vec) return integer is
        variable result : integer := 0;
    begin
        for i in v'range loop
            result := result + v(i);
        end loop;
        return result;
    end function;

    subtype rint is resolved integer;

    signal x : rint := 0;

    type rint_vec is array (natural range <>) of rint;

    signal y : rint_vec(1 to 2) := (0, 0);
    signal z : natural;
begin

    p1: process is
    begin
        x <= 1;
        assert x'driving_value = 0;
        wait for 1 ns;
        assert x = 3;
        assert x'driving;
        assert x'driving_value = 1;
        wait;
    end process;

    p2: process is
    begin
        x <= 2;
        assert x'driving_value = 0;
        wait for 1 ns;
        assert x'driving;
        assert x'driving_value = 2;
        wait;
    end process;

    p3: process is
    begin
        y <= (1, 2);
        wait for 1 ns;
        assert y = (6, 2);
        assert y'driving;
        assert y'driving_value = (1, 2);
        wait;
    end process;

    p4: process is
    begin
        y(1) <= 5;
        wait for 1 ns;
        assert not y'driving;
        wait;
    end process;

    p5: process is
    begin
        z <= 1;
        assert z = 0;
        assert z'driving_value = 0;
        wait for 1 ns;
        assert z'driving_value = 1;
        wait;
    end process;

end architecture;
