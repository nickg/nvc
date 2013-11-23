entity func14 is
end entity;

architecture test of func14 is
begin

    one: process is

        function func(x : integer) return integer is
        begin
            return x * 2;
        end function;

        variable y : integer;
    begin
        y := 2;
        wait for 1 ns;
        assert func(y) = 4;
        y := 4;
        wait for 1 ns;
        assert func(y) = 8;
        wait;
    end process;

    two: process is

        function func(x : integer) return integer is
        begin
            return x / 2;
        end function;

        variable y : integer;
    begin
        y := 2;
        wait for 1 ns;
        assert func(y) = 1;
        y := 4;
        wait for 1 ns;
        assert func(y) = 2;
        wait;
    end process;

end architecture;
