library ieee;
use ieee.std_logic_1164.all;

entity issue932 is
end entity issue932;

architecture test of issue932 is

    type t_slv_array is array(natural range <>) of std_logic_vector;

    function get_first_element (
        slv_array : t_slv_array
    ) return std_logic_vector is
        constant slv_array_normalized : t_slv_array(0 to slv_array'length-1) := slv_array;
    begin
        return slv_array_normalized(0);
    end function get_first_element;

    function get_first_element_var (
        slv_array : t_slv_array
    ) return std_logic_vector is
        variable slv_array_normalized : t_slv_array(0 to slv_array'length-1) := slv_array;
    begin
        return slv_array_normalized(0);
    end function get_first_element_var;

    function get_first_element_alias (
        slv_array : t_slv_array
    ) return std_logic_vector is
        alias slv_array_normalized : t_slv_array(0 to slv_array'length-1) is slv_array;
    begin
        return slv_array_normalized(0);
    end function get_first_element_alias;

    constant c_slv_array : t_slv_array(0 to 3)(7 downto 0) := (others=>(others=>'0'));
    signal slv_array_normalized_sig : t_slv_array(0 to c_slv_array'length-1) := c_slv_array;
begin

    p_nvc_bug : process
        variable v_slv_array : t_slv_array(0 to 3)(7 downto 0) := (others=>(others=>'0'));
    begin
        report "SLV array element 0 (const): " & to_hstring(get_first_element(v_slv_array(1 to 3)));
        report "SLV array element 0 (var): " & to_hstring(get_first_element_var(v_slv_array(1 to 3)));
        report "SLV array element 0 (alias): " & to_hstring(get_first_element_alias(v_slv_array(1 to 3)));      report "SLV array element 0 (signal): " & to_hstring(slv_array_normalized_sig(0));

        assert get_first_element(v_slv_array(1 to 3)) = X"00";
        assert get_first_element_var(v_slv_array(1 to 3)) = X"00";
        assert get_first_element_alias(v_slv_array(1 to 3)) = X"00";
        assert slv_array_normalized_sig(0) = X"00";

        wait;
    end process p_nvc_bug;

end architecture test;
