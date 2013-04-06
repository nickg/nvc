entity attr3 is
end entity;

architecture test of attr3 is

    type int_vec is array (integer range <>) of integer;

    procedure foo(x : in int_vec) is
    begin
        for i in x'reverse_range loop
            report integer'image(i);
        end loop;
    end procedure;

begin

    process is
        variable a : int_vec(1 to 5);
    begin
        for i in a'reverse_range loop
            report integer'image(i);
        end loop;
        foo(a);
        wait;
    end process;

end architecture;
