entity vhpi20 is
end entity;

architecture test of vhpi20 is
    type t1 is array (natural range <>) of integer;
    signal s1 : t1(1 to 3);
    signal s2 : integer := 42;

    type t2 is record
        x, y : integer;
        z : integer_vector(1 to 3);
    end record;
    signal s3 : t2 := (55, -77, (1, 2, 3));

    type t4 is record
        y : integer_vector(1 to 3);
        x : bit;
    end record; -- 13 bytes + 7 bytes padding
    type t4_array is array (natural range <>) of t4;

    type t3 is array (natural range <>) of t1;
    signal s4 : t3(1 to 2)(7 downto 6) := ((1, 2), (3, 4));
begin

    b1: block is
        generic (
            g1 : t4_array(0 to 1) := (((1, 2, 3), '0'),
                                       ((4, 5, 6), '1')) );
    begin
    end block;

end architecture;
