entity indexed_name_expressions_named_index is
end entity;

architecture test of indexed_name_expressions_named_index is
    type integer_array_t is array (natural range <>) of integer;
    signal values : integer_array_t(0 to 1);
begin
    process
    begin
        values(index => 0) <= 1;  -- Error
        wait;
    end process;
end architecture;

entity attribute_does_not_take_arguments is
end entity;

architecture test of attribute_does_not_take_arguments is
    subtype bounded_real is real range 0.0 to 1.0;
    signal source_value : bounded_real := 0.5;
    signal result : real;
begin
    result <= source_value'left(1);  -- Error
end architecture test;
