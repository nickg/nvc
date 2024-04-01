entity vhpi14 is
end entity;

architecture test of vhpi14 is
    function add2 (x : integer) return integer;
    attribute foreign of add2 : function is "VHPI lib add2";

    function popcount (x : bit_vector) return natural;
    attribute foreign of popcount : function is "VHPI lib popcount";

    type t_int_vec is array (natural range <>) of integer;

    procedure test1 (x : inout t_int_vec(1 to 4); y : in integer);
    attribute foreign of test1 : procedure is "VHPI lib test1";

    procedure test2 (x : inout t_int_vec; y : in integer);
    attribute foreign of test2 : procedure is "VHPI lib test2";

    function iota (n : natural) return t_int_vec;
    attribute foreign of iota : function is "VHPI lib iota";
begin

    p: process is
        variable v1 : t_int_vec(1 to 4) := (1, 2, 3, 4);
    begin
        assert add2(1) = 3;
        assert add2(-1) = 1;
        assert add2(-10) = -8;
        assert popcount("101") = 2;
        assert popcount("10111") = 4;

        test1(v1, 5);
        assert v1 = (6, 7, 8, 9);

        test2(v1, 1);
        assert v1 = (7, 8, 9, 10);

        v1 := (-4, -3, -2, -1);
        test2(v1, 2);
        assert v1 = (-2, -1, 0, 1);

        assert iota(2) = (0, 1);
        assert iota(4) = (0, 1, 2, 3);

        wait;
    end process;

end architecture;
