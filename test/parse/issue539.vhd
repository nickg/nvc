package my_package is
    type slv_1_t is array (natural range <>) of bit_vector;
    function addslvreg_f(arg0 : slv_1_t; arg1 : bit_vector) return slv_1_t;
end package my_package;

package body my_package is
    function addslvreg_f(arg0 : slv_1_t; arg1 : bit_vector) return slv_1_t is
        variable cb_v : bit_vector(0 downto 0);
        variable arg_v,retval : arg0'subtype;  -- OK
        variable foo : slv_1_t(arg0'range);  -- Error
        constant W : integer := arg0(0)'length-1;
    begin
        return retval;
    end function addslvreg_f;
end package body;
