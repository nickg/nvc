entity issue580 is
end entity;

architecture test of issue580 is
    type rec_t is record
        bits : bit_vector;
    end record;

    type rec_array_t is array (natural range <>) of rec_t;

    subtype sub_t is rec_array_t(open)(bits(1 to 3));  -- OK

    signal s1 : sub_t(1 to 4);          -- OK
    signal s2 : sub_t;                  -- Error
begin
end architecture;
