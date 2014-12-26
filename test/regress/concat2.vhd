entity concat2 is
end entity;

architecture test of concat2 is
    type int_array is array (integer range <>) of integer;

    -- Dummy argument is to prevent constant folding;

    function get_array(dummy : in integer) return int_array is
    begin
        return (1, 2, 3);
    end function;

    function pad(a : in int_array; dummy : in integer) return int_array is
    begin
        return (0, 0) & a;
    end function;

begin

    process is
        variable x : int_array(0 to 3);
        variable d : integer;
    begin
        x := get_array(d) & ( 0 => 4 );
        assert x = (1, 2, 3, 4);
        x := get_array(d) & ( 6 => 8 );
        assert x = (1, 2, 3, 8);
        x := pad((1, 2), d);
        assert x = (0, 0, 1, 2);
        wait;
    end process;

end architecture;
