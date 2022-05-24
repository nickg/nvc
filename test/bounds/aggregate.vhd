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

    constant c4 : my_bit_vec := (3 => '1', 2 => '0');  -- Error
    constant c5 : my_int_map := (7 => 2);  -- Error

    constant c6 : integer_vector(1 to 3) := (1, 2, 3);  -- OK
    constant c7 : integer_vector(1 to 4) := (c6, 4);  -- OK
    constant c8 : integer_vector(1 to 3) := (c6, 4);  -- Error
    constant c9 : integer_vector(1 to 5) := (c6, 4);  -- Error
    constant c10 : integer_vector(1 to 3) := (1 to 3 => c6);  -- OK
    constant c11 : integer_vector(1 to 3) := (1 to 2 => c6(1 to 2));  -- Error
    constant c12 : integer_vector(1 to 4) := (1 to 4 => c6);  -- Error
    constant c13 : integer_vector(1 to 4) :=
        ( integer_vector'(1, 2), integer_vector'(3, 4) );  -- OK
    constant c14 : bit_vector(7 downto 0) :=
        (3 downto 0 => "111", others => '0');   -- Error
begin

end architecture;
