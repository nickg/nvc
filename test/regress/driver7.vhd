entity driver7 is
end entity;

architecture test of driver7 is
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
begin

    p1: process is
    begin
        x <= 1;
        wait for 1 ns;
        assert x = 3;
        assert x'driving;
        assert x'driving_value = 1;
        wait;
    end process;

    p2: process is
    begin
        assert x'driving;               -- Error
        wait;
    end process;
end architecture;
