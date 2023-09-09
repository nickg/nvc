entity nullarray is
end entity;

architecture test of nullarray is
    subtype null_range_type is integer range 1 to -1;

    type rec is record
        x, y : integer;
        z : bit_vector(1 to 3);
    end record;

    type rec_array is array (natural range <>) of rec;

    function get_bits return bit_vector is
    begin
        return "010";
    end function;

    constant A : bit_vector := get_bits;

    constant B : rec_array(null_range_type) := (others => (0, 1, A));
begin
end architecture;
