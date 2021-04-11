entity attr1 is
end entity;

architecture test of attr1 is
    type my_int is range 10 downto 0;
begin

    process is
        variable x : integer;
        variable y : my_int;
    begin
        assert integer'left = -2147483648;
        x := integer'right;
        wait for 1 ns;
        assert x = 2147483647;
        assert positive'left = 1;
        assert natural'high = integer'high;
        assert integer'ascending;
        assert not my_int'ascending;
        x := 0;
        wait for 1 ns;
        assert integer'succ(x) = 1;
        assert integer'pred(x) = -1;
        x := 1;
        y := 1;
        wait for 1 ns;
        assert integer'leftof(x) = 0;
        assert integer'rightof(x) = 2;
        assert my_int'leftof(y) = 2;
        assert my_int'rightof(y) = 0;
        assert my_int'base'left = 10;
        wait;
    end process;

end architecture;
