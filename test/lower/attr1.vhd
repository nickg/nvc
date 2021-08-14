entity attr1 is
end entity;

architecture test of attr1 is
    type my_int is range 10 downto 0;
begin

    p1: process is
        variable x : integer := 0;
        variable y : my_int;
        variable z : integer := 1;
    begin
        assert integer'succ(x) = 1;
        assert integer'pred(x) = -1;
        assert integer'leftof(z) = 0;
        assert integer'rightof(z) = 2;
        assert my_int'leftof(y) = 2;
        assert my_int'rightof(y) = 0;
        wait;
    end process;

end architecture;
