entity transfer1 is
end entity;

architecture test of transfer1 is
    signal a, b, c : integer;
    signal d, e, f : bit_vector(1 to 3);
begin

    p1: a <= b;

    p2: d(1 to 2) <= e(2 to 3);

    p3: process (f) is
    begin
        e <= d;
    end process;

    p4: d(1) <= reject 2 ns inertial e(2) after 5 ns;

end architecture;
