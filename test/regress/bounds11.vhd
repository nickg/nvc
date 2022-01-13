entity bounds11 is
end entity;

architecture test of bounds11 is
    type my_int is range 0 to 100;
    signal i : my_int;
begin

    p1: i <= i + 1 after 10 ns;

end architecture;
