entity issue663 is
end entity;

architecture test of issue663 is
    function get_data return bit_vector is
    begin
        return "10";
    end function;

    function get_data return bit is
    begin
        return '1';
    end function;

    function get_zeros(n : integer) return bit_vector is
    begin
        return (1 to n => '0');
    end function;

    function get_zeros(n : integer) return bit is
    begin
        assert n = 1;
        return '0';
    end function;

    signal x, y : bit;
begin

    p1: process is
        variable a, b : bit;
    begin
        (a, b) := get_data;             -- OK
        a := get_data;                  -- OK
        (a, b) := get_zeros(2);         -- OK
        a := get_zeros(1);              -- OK
        (x, y) <= get_data;             -- OK
        x <= get_data;                  -- OK
        (x, y) <= get_zeros(2);         -- OK
        x <= get_zeros(1);              -- OK
        wait;
    end process;

end architecture;
