entity ee is end entity;

architecture aa of ee is
    type int_ptr is access integer;
    type bv_ptr is access bit_vector;
begin

    process is
        variable x, p : int_ptr;
        variable v : integer;
        variable a : bv_ptr;
        variable q : bit_vector(1 to 3);
        variable r : bit;
    begin
        x.all := 1;
        v := x.all + 5;
        p := new integer;
        q := a.all(1 to 3);
        r := a.all(3);
    end process;

end architecture;
