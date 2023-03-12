entity issue646 is
end entity;

architecture test of issue646 is

    function get_init (x : integer; y : bit_vector) return integer is
    begin
        assert x = 42;
        assert y = "000";
        return 55;
    end function;

    signal x : integer := 42;
    signal y : bit_vector(1 to 3);
    signal z : integer := get_init(x, y);
begin
    assert z = 55;
end architecture;
