-- LCS 2016-019
entity signal32 is
end entity;

architecture test of signal32 is
    type rec_t is record
        x : integer_vector;
    end record;

    function get_rec return rec_t is
    begin
        return (x => (5, 6));
    end function;

    signal s1 : integer_vector := (1, 2, 3);
    signal s2 : rec_t := get_rec;
begin

    p1: process is
        variable v1 : integer_vector := (1, 2, 3);
        variable v2 : rec_t := get_rec;
    begin
        assert s1'length = 3;
        assert s1'left = 0;
        assert s1(1) = 2;
        s1(2) <= 5;
        wait for 1 ns;
        assert s1 = (1, 2, 5);
        assert v1'length = 3;
        assert v1(2) = 3;
        assert s2.x'length = 2;
        assert s2.x = (5, 6);
        assert v2.x'length = 2;
        assert v2.x = (5, 6);
        v2.x(1) := 9;
        assert v2.x = (5, 9);
        wait;
    end process;

end architecture;
