entity func13 is
end entity;

architecture test of func13 is
begin

    process is
        variable x : integer;

        impure function add_to_x(y : integer) return integer is
        begin
            return x + y;
        end function;
    begin
        x := 2;
        assert add_to_x(5) = 7;
        x := 3;
        assert add_to_x(5) = 8;
        wait;
    end process;

end architecture;
