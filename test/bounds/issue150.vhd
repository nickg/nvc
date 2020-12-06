entity issue150 is
end entity issue150;

architecture tb of issue150 is
    type t_lut8x8 is array (0 to 7,0 to 7) of integer;

    constant C_SAMPLE : t_lut8x8 := (
        (1,1,1,1,1,1,1,1),
        (1,1,1,1,1,1,1,1),
        (1,1,1,1,1,1,1,1),
        (1,1,1,1,1,1,1,1),
        (1,1,1,1,1,1,1,1),
        (1,1,1,1,1,1,1,1) );            -- Crash lowering here
begin



end architecture;
