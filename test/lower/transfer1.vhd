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

    issue765: block is
        type a_rec is record
            b : bit_vector(1 downto 0);
            c : bit_vector(7 downto 0);
        end record a_rec;

        type a_rec_vect is array (integer range <>) of a_rec;

        signal s1 : a_rec_vect(1 downto 0);
        signal s2 : a_rec_vect(1 downto 0);
    begin
        p1: s2(1 downto 0) <= s1(1 downto 0);
        p2: s1 <= s2;
    end block;

end architecture;
