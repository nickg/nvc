entity bounds38 is
end entity;

architecture test of bounds38 is

    type int_vec_2d is array (natural range <>, natural range <>) of integer;

    function double (x : in int_vec_2d) return int_vec_2d is
        variable result : int_vec_2d(x'range(2), x'range(1));
        variable sum : integer;
    begin
        for i in result'range(1) loop
            for j in result'range(2) loop
                result(i, j) := x(i, j) * 2;  -- Error
            end loop;
        end loop;
        return result;
    end function;

    signal s3, s4 : int_vec_2d(1 to 2, 5 to 7) := (others => (others => 0));
begin

    p2: s4 <= double(s3);

end architecture;
