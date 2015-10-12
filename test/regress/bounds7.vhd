entity bounds7 is
end entity;

architecture test of bounds7 is
    type my_int is range 1 to 10;
begin

    process is
        variable x : my_int;
        variable y : integer;
    begin
        x := 6;
        y := integer(x);
        x := 10;
        x := x + 1;
        wait;
    end process;

end architecture;
