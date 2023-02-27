entity bounds39 is
end entity;

architecture test of bounds39 is
    type bv10 is array (1 to 10) of bit;
begin

    p1: process is
        variable a : bit_vector(1 to 10);
        variable b : bv10;
        variable n : integer;
    begin
        n := 9;
        wait for 1 ns;
        b := bv10(a);
        b := bv10(a(1 to n));           -- Error
        wait;
    end process;

end architecture;
