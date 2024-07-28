library ieee;
use ieee.std_logic_1164.all;

entity bounds45 is
end entity;

architecture test of bounds45 is

    type t_slv_array is array(natural range <>) of std_logic_vector;

    function get_first_element (
        slv_array : t_slv_array
    ) return std_logic_vector is
        constant slv_array_normalized : t_slv_array(0 to slv_array'length*2) := slv_array;
--        alias slv_array_normalized : t_slv_array(0 to slv_array'length*2) is slv_array;
        -- Error
    begin
        return slv_array_normalized(slv_array'length*2 - 1);
    end function get_first_element;

begin

    p_nvc_bug : process
        variable v_slv_array : t_slv_array(0 to 3)(7 downto 0) := (others=>(others=>'0'));
    begin
        report "SLV array element 0 (const): " & to_hstring(get_first_element(v_slv_array(1 to 3)));

        wait;
    end process p_nvc_bug;

end architecture test;
