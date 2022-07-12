entity record34 is
end entity;

architecture test of record34 is
    type rec is record
        x : bit_vector;
        y : natural;
    end record;

    type rec_array is array (natural range <>) of rec;

    type wrapper is record
        f : rec_array(1 to 3)(x(1 to 2));
    end record;

    signal s : wrapper;
begin

    p1: process is
    begin
        s.f(1) <= (x => "10", y => 1);
        wait for 1 ns;
        assert s.f = (1 => ("10", 1), 2 to 3 => ("00", 0));

        s.f(2).x(1) <= '1';
        wait for 1 ns;
        assert s.f = (1 => ("10", 1), 2 => ("10", 0), 3 => ("00", 0));

        wait;
    end process;

end architecture;
