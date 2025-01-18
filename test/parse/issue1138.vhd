entity test is
end entity test;

architecture rtl of test is

    subtype slv64_type is bit_vector(0 to 63);
    type    slv64_array_type is array (natural range <>) of slv64_type;

    constant zeroes : bit_vector(0 to 31)  := x"00000000";
    constant more_zeroes : bit_vector(0 to 31) := x"00000000";

    -- doesn't work in nvc 1.15.0, works in nvc 1.14.2
    -- error: ambiguous use of operator "&"
    constant test_constant : slv64_array_type := (zeroes & more_zeroes, zeroes & more_zeroes);

begin
end architecture rtl;
