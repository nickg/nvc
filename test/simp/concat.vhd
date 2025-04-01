package concat is
    constant c1 : bit_vector := "1" & "00";
    constant c2 : string := "xy" & "z";
    constant c3 : bit_vector := '1' & '0';
    constant c4 : string := "fo" & 'o';

    constant c5 : integer_vector := 0 & 1;  -- (A_POS, A_POS)
    constant c6 : integer_vector := (0 & 1) & 2;  -- (A_POS, A_POS, A_POS)
    constant c7 : integer_vector := c5 & 2;  -- (A_CONCAT, A_POS)
    constant c8 : integer_vector := 0 & (1 & 2);  -- (A_POS, A_POS, A_POS)
    constant c9 : integer_vector := 1 & c8;  -- (A_POS, A_CONCAT)
    constant c10 : integer_vector := c5 & c6;  -- (A_CONCAT, A_CONCAT)
    constant c11 : integer_vector := 1 & 2 & 3 & 4;  -- (A_POS, A_POS, A_POS, A_POS)
end package;
