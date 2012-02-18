entity null1 is
end entity;

architecture test of null1 is

    type int_array is array (integer range <>) of integer;
        
    function get_null return int_array is
        variable b : int_array(7 to -999999) := (others => 0);
    begin
        return b;
    end function;
    
begin

    process is
        variable a : int_array(0 to -1) := (others => 0);
        variable b : int_array(7 to -999999) := (others => 0);
        variable c : int_array(0 downto 1) := (others => 0);
    begin
        report integer'image(a'length);
        assert a'length = 0;
        report integer'image(b'length);
        assert b'length = 0;
        report integer'image(c'length);
        assert c'length = 0;
        a := get_null;
        wait;
    end process;

end architecture;
