entity attr8 is
end entity;

architecture test of attr8 is
begin

    process is
        type myint is range 1 to 3;
        subtype myint_sub is myint range 1 to 2;
        variable x : integer;
    begin
        assert myint'val(1) = 1;
        assert myint'val(2) = 2;
        x := 1;
        assert myint'val(x) = 1;
        x := 2;
        assert myint'val(x) = 2;
        assert myint_sub'val(2) = 2;
        assert myint_sub'val(x) = 2;
        assert myint'pos(myint(x)) = x;
        assert myint_sub'pos(myint(x)) = x;
        assert myint'pos(1) = 1;
        assert myint_sub'pos(1) = 1;
        wait;
    end process;

end architecture;
