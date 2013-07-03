entity bounds is
end entity;

architecture test of bounds is
    type foo is range 1 to 5;
    type my_vec1 is array (positive range <>) of integer;
    type my_vec2 is array (foo range <>) of integer;
begin

    process is
        variable a : my_vec1(0 to 10);  -- Error
        variable b : my_vec2(1 to 60);  -- Error
    begin
    end process;

end architecture;
