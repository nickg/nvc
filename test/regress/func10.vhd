entity func10 is
end entity;

architecture test of func10 is

    type int3d is array (natural range <>,
                         natural range <>,
                         natural range <>) of integer;

    function func1(x, y, z : in integer) return integer is
        variable r : int3d(1 to x, 1 to y, 1 to z);
    begin
        return 1;
    end function;

begin

    process is
        variable x, y, z : integer;
    begin
        x := 1;
        y := 1;
        z := 2;
        wait for 1 ns;
        assert func1(x, y, z) = 1;
        wait;
    end process;

end architecture;
