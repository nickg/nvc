entity agg8 is
end entity;

architecture test of agg8 is
    type rec is record
        x : integer;
        y : bit_vector;
    end record;

    type rec_array is array (natural range <>) of rec;

    signal s : rec(y(1 to 3)) := ( 1, "101" );
    signal t : rec_array(1 to 2)(y(1 to 3));
begin

    p1: process is
        variable v : t'subtype;
    begin
        t <= (others => s);
        wait for 1 ns;
        assert t(2).x = 1;
        assert t(1).y = "101";
        t(2) <= (2, "110");
        wait for 1 ns;

        v := t;
        assert v(1).x = 1;
        assert v(2).y = "110";

        v(1 to 2) := t(1 to 2);
        assert v(1).x = 1;
        assert v(2).y = "110";

        wait;
    end process;

end architecture;
