entity attr15 is
end entity;

architecture test of attr15 is

    function double (x : in integer_vector) return integer_vector is
        variable result : x'subtype;
    begin
        for i in result'range loop
            result(i) := x(i) * 2;
        end loop;
        return result;
    end function;

    type int_vec_2d is array (natural range <>, natural range <>) of integer;

    function double (x : in int_vec_2d) return int_vec_2d is
        variable result : x'subtype;
    begin
        for i in result'range(1) loop
            for j in result'range(2) loop
                result(i, j) := x(i, j) * 2;
            end loop;
        end loop;
        return result;
    end function;

    signal s1, s2 : integer_vector(1 to 5) := (others => 0);
    signal s3, s4 : int_vec_2d(1 to 2, 5 to 5) := (others => (others => 0));
begin

    p1: s2 <= double(s1);

    p2: s4 <= double(s3);

    check: process is
    begin
        s1 <= (1, 2, 3, 4, 5);
        wait for 1 ns;
        assert s2 = (2, 4, 6, 8, 10);

        s3 <= ( (5 => 1), (5 => 2) );
        wait for 1 ns;
        assert s4 = ( (5 => 2), (5 => 4) );

        wait;
    end process;

end architecture;
