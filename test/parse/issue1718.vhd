package issue1718 is
    constant c1 : string := 4sx"F0";  -- Error
    constant c2 : string := 7sx"7F";  -- Error
    constant c3 : string := sd"5";    -- Error
    constant c4 : string := ud"5";    -- Error
end package;
