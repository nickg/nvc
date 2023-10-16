entity conv1 is
end entity;

architecture test of conv1 is
    type natural_vector is array (natural range <>) of natural;
    type nat3_vector is array (1 to 3) of natural;

    type int2d is array (natural range <>, natural range <>) of integer;
    type nat2d is array (natural range <>, natural range <>) of natural;

    type byte_array1 is array (natural range <>) of bit_vector(1 to 8);
    type byte_array2 is array (natural range <>) of bit_vector(7 downto 0);
begin

    p1: process is
        constant c1 : natural_vector(1 to 3) := natural_vector(integer_vector'(1, 2, 3));
        constant c2 : nat3_vector := nat3_vector(integer_vector'(1, 2, 3));
        constant c3 : nat2d := nat2d(int2d'((1, 2), (3, 4)));
        constant c4 : byte_array1 := byte_array1(byte_array2'(X"01", X"02"));
    begin
    end process;

end architecture;
