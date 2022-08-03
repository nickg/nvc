entity agg9 is
end entity;

architecture test of agg9 is
    type rec is record
        x, y : natural;
    end record;

    type rec_array is array (natural range <>) of rec;

    signal s : rec_array(1 to 2);
    signal r1, r2 : rec;
begin

    s <= (1 => r1, 2 => r2);

    process is
    begin
        assert s = ((0, 0), (0, 0));
        r2 <= (2, 4);
        wait for 1 ns;
        assert s = ((0, 0), (2, 4));
        r1.x <= 7;
        wait for 1 ns;
        assert s = ((7, 0), (2, 4));
        wait;
    end process;

end architecture;
