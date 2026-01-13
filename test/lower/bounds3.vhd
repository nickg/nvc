entity bounds3 is
end entity;

architecture test of bounds3 is
    type rec1 is record
        x : bit;
        y : bit_vector(1 to 3);
    end record;

    signal a : bit range '0' to '0';
    signal b : bit_vector(1 to 2);
begin

    p1: process is
        variable r1 : rec1;
    begin
        (a, b) <= r1;
        wait;
    end process;

end architecture;
