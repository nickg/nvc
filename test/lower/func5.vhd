entity func5 is
end entity;

architecture test of func5 is

    function add_one_s(signal s : in integer) return integer is
    begin
        return s + 1;
    end function;

    function event(signal s : in integer) return boolean is
    begin
        return s'event;
    end function;

    signal x : integer := 5;
begin

    p1: process is
    begin
        assert add_one_s(x) = 6;
        assert event(x);
        wait;
    end process;

end architecture;
