entity cond5 is
end entity;

architecture test of cond5 is
    type rec is record
        x, y : integer;
    end record;

    constant c0 : integer := 10 when cond5'path_name = ":cond5:" else 5;
    constant c1 : bit_vector(1 to 3) := "101" when cond5'path_name = ":cond5:" else "111";
    constant c2 : rec := (1, 2) when cond5'path_name = ":cond5:xxxx"
                         else (5, 6) when cond5'path_name = ":cond5:yyy" else (7, 8);
begin

    p1: process is
    begin
        assert c0 = 10;
        assert c1 = "101";
        assert c2 = (7, 8);
        wait;
    end process;

end architecture;
