entity vhpi14 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of vhpi14 is
    function add2 (x : integer) return integer is
    begin
    end function;

    attribute foreign of add2 : function is "VHPI lib add2";

    function popcount (x : bit_vector) return natural is
    begin
    end function;

    attribute foreign of popcount : function is "VHPI lib popcount";

    type t_int_vec is array (natural range <>) of integer;

    procedure test1 (x : inout t_int_vec(1 to 4); y : in integer) is
    begin
    end procedure;

    attribute foreign of test1 : procedure is "VHPI lib test1";

    procedure test2 (x : inout t_int_vec; y : in integer) is
    begin
    end procedure;

    attribute foreign of test2 : procedure is "VHPI lib test2";

    procedure test3 (x : inout integer; y : in integer) is
    begin
    end procedure;

    attribute foreign of test3 : procedure is "VHPI lib test3";

    procedure test4 (x : inout integer; variable y : in integer) is
    begin
    end procedure;

    attribute foreign of test4 : procedure is "VHPI lib test4";

    function iota (n : natural) return t_int_vec is
    begin
    end function;

    attribute foreign of iota : function is "VHPI lib iota";

    procedure no_args is                -- Issue #984
    begin
    end procedure;

    attribute foreign of no_args : procedure is "VHPI lib no_args";

    procedure set_logic (x : out std_ulogic; dummy : in std_logic) is
    begin
    end procedure;

    attribute foreign of set_logic : procedure is "VHPI lib set_logic";
begin

    p: process is
        variable v1 : t_int_vec(1 to 4) := (1, 2, 3, 4);
        variable v2 : std_ulogic;
        variable v3 : integer;
        variable v4 : integer;
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

        v3 := 1;
        test3(v3, 2);
        assert v3 = 3;

        v3 := 2;
        test3(v3, 2);
        assert v3 = 4;

        v3 := -4;
        test3(v3, 2);
        assert v3 = -2;

        v3 := -2;
        test3(v3, 2);
        assert v3 = 0;

        v3 := -1;
        test3(v3, 2);
        assert v3 = 1;

        v3 := 1;
        v4 := -1;
        test4(v3, v4);
        assert v3 = 0;

        v3 := 2;
        v4 := 2;
        test4(v3, v4);
        assert v3 = 4;

        v3 := -4;
        v4 := 3;
        test4(v3, v4);
        assert v3 = -1;

        v3 := -2;
        v4 := 4;
        test4(v3, v4);
        assert v3 = 2;

        v3 := -1;
        v4 := 5;
        test4(v3, v4);
        assert v3 = 4;

        assert iota(2) = (0, 1);
        assert iota(4) = (0, 1, 2, 3);

        no_args;

        set_logic(v2, 'X');
        assert v2 = '1';

        wait;
    end process;

end architecture;
