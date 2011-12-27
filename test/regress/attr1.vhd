entity attr1 is
end entity;

architecture test of attr1 is
    type my_int is range 10 downto 0;
begin

    process is
        variable x : integer;
    begin
        assert integer'left = -2147483648;
        x := integer'right;
        assert x = 2147483647;
        assert positive'left = 1;
        assert natural'high = integer'high;
        assert integer'ascending;
        assert not my_int'ascending;
        assert integer'succ(0) = 1;
        assert integer'pred(0) = -1;
        assert integer'leftof(1) = 0;
        assert integer'rightof(1) = 2;
        assert my_int'leftof(1) = 2;
        assert my_int'rightof(1) = 0;
        wait;
    end process;
    
end architecture;
