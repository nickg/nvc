entity proc2 is
end entity;

architecture test of proc2 is

    type int_array is array (integer range <>) of integer;
    
    procedure fill(a : out int_array) is
    begin
        for i in a'range loop
            a(i) := a'length;
        end loop;
    end procedure;
    
begin

    process is
        variable x : int_array(1 to 3);
        variable y : int_array(5 to 6);
    begin
        fill(x);
        assert x = (3, 3, 3);
        fill(y);
        assert y = (2, 2);
        wait;
    end process;

end architecture;
