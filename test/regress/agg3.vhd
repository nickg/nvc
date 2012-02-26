entity agg3 is
end entity;

architecture test of agg3 is
    type int_array is array (integer range <>) of integer;

    function get_array return int_array is
    begin
        return (4 => 4, 3 => 3, 5 => 5);
    end function;
begin

    process is
        variable x : int_array(1 to 3) := (others => 5);
        variable y : integer;
    begin
        x := (6 => 7) & (6 => 2, 7 => 9);
        assert x = (7, 2, 9);
        x := get_array;
        assert x = (3, 4, 5);
        wait;
    end process;

end architecture;
