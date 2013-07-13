entity bounds3 is
end entity;

architecture test of bounds3 is

    type int_vec is array (natural range <>) of integer;

    signal s : int_vec(5 to 7);
    signal k : integer;
begin

    s(k) <= 61;

end architecture;
