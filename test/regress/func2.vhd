entity func2 is
end entity;

architecture rtl of func2 is

    type int_array is array (integer range <>) of integer;

    function len(x : int_array) return integer is
    begin
        return x'length;
    end function;

    function sum(x : int_array) return integer is
        variable tmp : integer := 0;
    begin
        for i in x'range loop
            tmp := tmp + x(i);
        end loop;
        return tmp;
    end function;

    function asc(x : int_array) return boolean is
    begin
        return x'ascending;
    end function;

    function get_low(x : int_array) return integer is
    begin
        return x'low;
    end function;

    function get_high(x : int_array) return integer is
    begin
        return x'high;
    end function;

begin

    process is
        variable u : int_array(5 downto 1) := (6, 3, 1, 1, 2);
        variable v : int_array(1 to 5) := (3, 5, 6, 1, 2);
    begin
        assert len(v) = 5;
        assert sum(v) = 17;
        assert len(u) = 5;
        assert sum(u) = 13;
        assert asc(v);
        assert get_low(u) = 1;
        assert get_low(v) = 1;
        assert get_high(u) = 5;
        assert get_high(v) = 5;
        assert not asc(u);
        wait;
    end process;

end architecture;
