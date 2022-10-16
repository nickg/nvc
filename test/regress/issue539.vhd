package my_package is
    type slv_1_t is array (natural range <>) of bit_vector;
    function addslvreg_f(arg0 : slv_1_t; arg1 : bit_vector) return slv_1_t;
end package my_package;

package body my_package is
    function addslvreg_f(arg0 : slv_1_t; arg1 : bit_vector) return slv_1_t is
	variable cb_v : bit_vector(0 downto 0);
	variable arg_v,retval : arg0'subtype;
	constant W : integer := arg0(0)'length-1;
    begin
	return retval;
    end function addslvreg_f;
end package body;

-------------------------------------------------------------------------------

entity issue539 is
end entity;

use work.my_package.all;

architecture test of issue539 is

    function get_elt_left (x : slv_1_t) return integer is
    begin
	return x(x'left)'left;
    end function;

    function get_elt_left_2 (x : slv_1_t) return integer is
    begin
	return x'element'left;
    end function;

begin

    p1: process is
	variable v1 : slv_1_t(0 to 3)(5 to 6);
	variable v2 : slv_1_t(0 to 3)(6 to 5);
    begin
	assert v1(1)'left = 5;
	assert v2(5)'left = 6;
	assert get_elt_left(v1) = 5;
	assert get_elt_left(v2) = 6;
	assert get_elt_left_2(v1) = 5;
	assert get_elt_left_2(v2) = 6;

	assert addslvreg_f(v1, "101") = v1;

	wait;
    end process;

end architecture;
