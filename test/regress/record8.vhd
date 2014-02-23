entity record8 is
end entity;

architecture test of record8 is

    type small_int is range 0 to 5;

    type sub_rec is record
        var : small_int;
    end record;

    type rec is record
        vec : bit_vector(1 to 3);
        num : integer;
        sub : sub_rec;
    end record;

    signal r : rec;

begin

    process is
    begin
        assert r = ("000", integer'left, ( var => 0 ) );
        r.vec <= "101";
        wait for 1 ns;
        assert r = ("101", integer'left, ( var => 0 ) );
        assert r.vec = "101";
        assert r.vec(3) = '1';
        r.num <= 5;
        wait for 1 ns;
        assert r.num = 5;
        r.sub.var <= 2;
        wait for 1 ns;
        assert r.sub.var = 2;
        wait;
    end process;

end architecture;
