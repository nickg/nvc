entity alias1 is
end entity;

architecture test of alias1 is
    type int_array is array (integer range <>) of integer;
begin

    process is
        variable x : int_array(7 downto 4) := (1, 2, 3, 4);
        alias y : int_array(1 to 4) is x;
        alias z : int_array(5 downto 2) is x;
    begin
        report "--- X1 ---";
        for i in x'range loop
            report integer'image(x(i));
        end loop;
        report "--- Y1 ---";
        for i in y'range loop
            report integer'image(y(i));
        end loop;
        report "--- Z1 ---";
        for i in z'range loop
            report integer'image(z(i));
        end loop;
        wait;
    end process;

    process is
        variable x : int_array(4 to 7) := (1, 2, 3, 4);
        alias y : int_array(4 downto 1) is x;
        alias z : int_array(2 to 5) is x;
    begin
        wait for 1 ns;
        report "--- X2 ---";
        for i in x'range loop
            report integer'image(x(i));
        end loop;
        report "--- Y2 ---";
        for i in y'range loop
            report integer'image(y(i));
        end loop;
        report "--- Z2 ---";
        for i in z'range loop
            report integer'image(z(i));
        end loop;
        wait;
    end process;
    
end architecture;
