entity proc8 is
end entity;

architecture test of proc8 is

    type int_vec is array (integer range <>) of integer;

    subtype int_vec4 is int_vec(1 to 4);

    procedure p1(signal y : in int_vec) is
    begin
        for i in y'range loop
            report integer'image(y(i));
        end loop;
    end procedure;

    procedure p1b(variable y : in int_vec4) is
    begin
        for i in y'range loop
            report integer'image(y(i));
        end loop;
    end procedure;

    procedure p2(signal x : in int_vec4) is
    begin
        p1(x);
    end procedure;

    procedure p3(signal x : out int_vec) is
    begin
        x <= (6, 7, 8, 9);
    end procedure;

    signal s : int_vec4 := (1, 2, 3, 4);

begin

    process is
        variable k : int_vec4 := (-1, -2, -3, -4);
    begin
        p2(s);
        p3(s);
        wait for 1 ns;
        assert s = (6, 7, 8, 9);
        p1b(k);
        wait;
    end process;

end architecture;
