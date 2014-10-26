entity e is
end entity;

architecture test of e is

    alias my_int is integer;            -- OK
    signal x : my_int;                  -- OK
    subtype s is my_int range 1 to 5;   -- OK

    alias my_bad : integer is integer;  -- Error
    alias ax is x;                      -- OK
    signal y : ax;                      -- Error

    alias as is s;                      -- OK
    signal z : as;                      -- OK

begin

end architecture;
