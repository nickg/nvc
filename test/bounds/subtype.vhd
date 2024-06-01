package pack is
    type int8 is range 0 to 255;
    subtype int8_sub1 is int8 range -1 to 200;  -- Error
    subtype int8_sub2 is int8 range 100 to 300;  -- Error

    type abc is (a, b, c);
    subtype ab is abc range a to b;     -- OK
    subtype bc is ab range b to c;      -- Error

    type real10 is range 0.0 to 10.0;
    subtype real10_sub1 is real10 range -1.0 to 5.0;  -- Error

    type t_array is array (positive range -1 to 8) of bit;  -- Error
end package;
