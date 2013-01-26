entity bounds3 is
end entity;

architecture test of bounds3 is

    type int_vec is array (natural range <>) of integer;

    signal s : int_vec(5 to 7);
begin

    s(76272) <= 61;

end architecture;
