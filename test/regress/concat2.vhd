entity concat2 is
end entity;

architecture test of concat2 is
    type int_array is array (integer range <>) of integer;
    
    function get_array return int_array is
    begin
        return (1, 2, 3);
    end function;
    
begin

    process is
        variable x : int_array(0 to 3);
    begin
        x := get_array & ( 0 => 4 );
        assert x = (1, 2, 3, 4);
        wait;
    end process;

end architecture;
