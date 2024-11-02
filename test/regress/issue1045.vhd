entity issue1045 is
end entity;

architecture test of issue1045 is
    type t_rec is record
        b : bit_vector(1 to 3);
        i : integer;
    end record;

    signal s : t_rec;
begin

    s <= ("101", 5) after 1 ns,
         ("111", 5) after 2 ns,
         ("000", 6) after 3 ns,
         ("010", 999) after 4 ns;

end architecture;
