entity driver14 is
end entity;

architecture test of driver14 is

    type rec is record
        x, y : natural;
    end record;

    type rec_array is array (natural range <>) of rec;

    function resolved (x : rec_array) return rec is
    begin
        return x(0);                    -- Returns pointer to its argument
    end function;

    subtype rrec is resolved rec;

    signal s : rrec;
begin

    p1: s <= (1, 2);
    p2: s <= (3, 4);

    p3: process is
    begin
        assert s = (0, 0);
        wait for 0 ns;
        assert s = (1, 2) or s = (3, 4);
        wait;
    end process;

end architecture;
