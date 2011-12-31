entity alias2 is
end entity;

architecture test of alias2 is
    type int_array is array (integer range <>) of integer;

    function print(x : int_array) return integer is
        alias y : int_array(1 to x'length) is x;
        alias z : int_array(x'length downto 1) is x;
    begin
        report "--- X ---";
        for i in x'range loop
            report integer'image(x(i));
        end loop;
        report "--- Y ---";
        for i in y'range loop
            report integer'image(y(i));
        end loop;
        report "--- Z ---";
        for i in z'range loop
            report integer'image(z(i));
        end loop;
        return 0;
    end function;
begin

    process is
        variable x : int_array(7 downto 4) := (1, 2, 3, 4);
        variable dummy : integer;
    begin
        dummy := print(x);
        wait;
    end process;

    process is
        variable x : int_array(4 to 7) := (1, 2, 3, 4);
        variable dummy : integer;
    begin
        wait for 1 ns;
        dummy := print(x);
        wait;
    end process;
    
end architecture;
