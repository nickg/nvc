entity alias3 is
end entity;

architecture test of alias3 is
    type int_array is array (integer range <>) of integer;

    function cut(x : int_array; low, high: integer) return int_array is
        alias a : int_array(1 to x'length) is x;
    begin
        return a(low to high);
    end function;

    signal s : int_array(1 to 5) := (1, 2, 3, 4, 5);
begin

    process is
        variable x : int_array(1 to 5) := (1, 2, 3, 4, 5);
        variable y : int_array(4 downto 0) := (4, 3, 2, 1, 0);
        alias sa : int_array(4 downto 0) is x;
    begin
        assert x(2 to 4) = (2, 3, 4);
        assert sa(3 downto 1) = (2, 3, 4);
        assert cut(x, 2, 3) = (2, 3);
        assert cut(y, 1, 2) = (4, 3);
        wait;
    end process;

end architecture;
