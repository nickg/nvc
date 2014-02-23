entity record8 is
end entity;

architecture test of record8 is

    type rec is record
        vec : bit_vector(1 to 3);
        num : integer;
    end record;

    signal r : rec;

begin

    process is
    begin
        assert r = ("000", integer'left);
        r.vec <= "101";
        wait for 1 ns;
        assert r = ("101", integer'left);
        assert r.vec = "101";
        assert r.vec(3) = '1';
        r.num <= 5;
        wait for 1 ns;
        assert r.num = 5;
        wait;
    end process;

end architecture;
