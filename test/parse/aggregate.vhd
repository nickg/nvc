package aggregate is
    type int_vec_up is array (natural range <>) of integer;

    type down_natural is range 100 downto 0;
    type int_vec_down is array (down_natural range <>) of integer;

    constant c1 : int_vec_up(1 to 2) := (1, 2);    -- (1 to 2);
    constant c2 : int_vec_up(7 to 8) := (1 => 1, 2 => 2);    -- (1 to 2);
    constant c3 : int_vec_up := (1, 2);    -- (0 to 1);
    constant c4 : int_vec_down := (1, 2, 3);  -- (100 downto 98)
    constant c5 : int_vec_down(7 downto 6) := (1 => 1, 2 => 2);  -- (2 downto 1)
    constant c6 : int_vec_up := (c1, c1);  -- (0 to 3)
    constant c7 : int_vec_up := (1, c1);   -- (0 to 2)
end package;
