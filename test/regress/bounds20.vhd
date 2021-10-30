entity bounds20 is
end entity;

architecture test of bounds20 is
    type rec1 is record
        x : bit;
        y : bit_vector(1 to 3);
        z : integer;
    end record;

    signal a : bit;
    signal b : bit_vector(1 to 2);
    signal c : integer;
begin

    p1: process is
        variable r1 : rec1;
    begin
        r1 := ('1', "010", 42);
        (a, b, c) <= r1;
        wait;
    end process;

end architecture;
