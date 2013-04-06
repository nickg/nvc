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

    procedure p2(signal x : in int_vec4) is
    begin
        p1(x);
    end procedure;

    signal s : int_vec4 := (1, 2, 3, 4);

begin

    process is
    begin
        p2(s);
        wait;
    end process;

end architecture;
