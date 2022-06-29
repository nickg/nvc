entity signal22 is
end entity;

architecture test of signal22 is

    function check_event (signal x : bit_vector) return boolean is
    begin
        return x'event;
    end function;

    function get_last_value (signal x : bit_vector) return bit_vector is
    begin
        return x'last_value;
    end function;

    signal s : bit_vector(1 to 3);
begin

    s <= "101" after 1 ns, "011" after 2 ns;


    p1: process is
    begin
        assert not check_event(s);
        wait for 1 ns;
        assert check_event(s);
        assert get_last_value(s) = "000";
        wait for 1 ns;
        assert check_event(s);
        assert get_last_value(s) = "101";
        wait for 0 ns;
        assert not check_event(s);
        wait;
    end process;

end architecture;
