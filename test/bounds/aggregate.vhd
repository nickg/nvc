entity aggregate is
end entity;

architecture test of aggregate is
    type my_enum is (A, B, C);
    type my_enum_map is array (my_enum) of integer;

    constant c1 : my_enum_map := (A => 1, A => 2, C => 3);  -- Error
    constant c2 : my_enum_map := my_enum_map'(A => 1, B => 2);  -- Error
    constant c3 : my_enum_map := (b => 1, a to c => 2);  -- Error

    subtype my_bit_vec is bit_vector(3 downto 1);
    type my_int is range 10 downto 5;
    type my_int_map is array (my_int) of integer;

    --constant c3 : my_bit_vec := (3 => '1', 2 => '0');  -- Error
    --constant c4 : my_int_map := (7 => 2);  -- Error
begin

end architecture;
