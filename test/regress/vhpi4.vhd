entity vhpi4 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of vhpi4 is

    function sum (x, y : integer) return integer;
    attribute foreign of sum : function is "VHPIDIRECT __vhpi_sum";

    type int_vec is array (natural range <>) of integer;
    function sum_array (a : int_vec; len : integer) return integer;
    attribute foreign of sum_array : function is "VHPIDIRECT __vhpi_sum_array";

    function my_not (x : std_logic) return std_logic is
    begin
        report "do not call this" severity failure;
    end function;

    attribute foreign of my_not : function is "VHPIDIRECT __vhpi_my_not";

    procedure test_proc (x : out integer; arr : out int_vec);
    attribute foreign of test_proc : procedure is "VHPIDIRECT __vhpi_test_proc";

    procedure test_proc (x : out integer; arr : out int_vec) is
    begin
        report "do not call this" severity failure;
    end procedure;

begin

    main: process is
        variable i : integer;
        variable v : int_vec(1 to 3);
    begin
        assert sum(2, 3) = 5;
        assert sum_array(int_vec'(1, 2, 3, 4, 5), 5) = 15;
        assert my_not('1') = '0';
        assert my_not('0') = '1';
        assert my_not('U') = 'U';

        test_proc(i, v);
        assert i = 42;
        assert v = (integer'left, 5, integer'left);

        wait;
    end process;

end architecture;
