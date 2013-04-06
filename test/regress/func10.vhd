entity func10 is
end entity;

architecture test of func10 is

    type int3d is array (natural range <>,
                         natural range <>,
                         natural range <>) of integer;

    function func1(x, y, z : in integer) return integer is
        variable r : int3d(1 to x, 1 to y, 1 to z);
    begin
        for i in 1 to x loop
            for j in 1 to y loop
                for k in 1 to z loop
                    r(i, j, k) := i + j + k;
                end loop;
            end loop;
        end loop;
        return r(x / 2, y / 2, z / 2);
    end function;

begin

    process is
        variable x, y, z : integer;
    begin
        x := 4;
        y := 5;
        z := 6;
        wait for 1 ns;
        assert func1(x, y, z) = 7;
        wait;
    end process;

end architecture;
