entity proc7 is
end entity;

architecture test of proc7 is

    procedure foo(signal b : in bit_vector; i : in integer) is
        variable c : bit_vector(b'range);
    begin
        assert c(c'left) = '0';
        --wait for 1 ns;
        for i in b'range loop
            c(i) := b(i);
        end loop;
        wait for 1 ns;
        for i in c'range loop
            report bit'image(c(i));
        end loop;
    end procedure;

    signal s : bit_vector(1 to 3) := ( '1', '0', '1' );

begin

    process is
    begin
        s <= "110";
        foo(s, 1);
        foo(s, 1);
        wait;
    end process;

end architecture;
