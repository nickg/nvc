entity record17 is
end entity;

architecture test of record17 is
    type rec is record
        x : boolean;
        y : natural;
        z : real;
    end record;

    type rec_3x3 is array (1 to 3, 1 to 3) of rec;

    signal s : rec_3x3;
begin

    p1: process is
    begin
        assert s(2, 2).z = real'left;
        s(2, 2).y <= 123;
        wait for 1 ns;
        assert s(2, 2) = (false, 123, real'left);
        s(3, 3) <= (true, 456, 1.0);
        wait for 1 ns;
        assert s(3, 3).x = true;
        wait;
    end process;

end architecture;
