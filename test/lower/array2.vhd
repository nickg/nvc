entity array2 is
end entity;

architecture test of array2 is
    type int_vec is array (natural range <>) of integer;
    type int_vec_ptr is access int_vec;
    type int_vec2x2 is array (natural range <>, natural range <>) of integer;
    signal s : int_vec(1 to 2);
begin

    p1: process is
        variable v : int_vec(1 to 2);
        variable x : integer;
    begin
        v := (x, x);
        wait;
    end process;

    p2: process is
        variable v : int_vec(1 to 2);
        variable x : integer;
    begin
        v := (1 => x, others => 0);
        wait;
    end process;

    p3: process is
        variable v    : int_vec2x2(1 to 2, 1 to 2);
        variable x, y : integer;
    begin
        v := ((x, x), (y, y));
        wait;
    end process;

    p4: process is
        variable v    : int_vec2x2(1 to 2, 1 to 2);
        variable x, y : integer;
    begin
        v := ((x, x), others => (y, y));
        wait;
    end process;

    p5: process is
        variable v : int_vec(1 to 3);
        variable x : integer;
    begin
        v := (1 => 1, 2 => x, 3 => 3);
        wait;
    end process;

    p6: process is
        variable x : integer;
    begin
        s <= (x, x);
        wait;
    end process;

    p7: process is
        variable p : int_vec_ptr;
    begin
        p := new int_vec(1 to 3);
        wait;
    end process;

    p8: process is
        variable p : int_vec_ptr;
        variable x : integer;
    begin
        p := new int_vec'(x, x);
        wait;
    end process;

    p9: process is
        function f return int_vec is
        begin
            return (1, 2, 3);
        end function;
        variable p : int_vec_ptr;
        variable v : int_vec(1 to 2);
    begin
        p := new int_vec'(f, 1);
    end process;

end architecture;
