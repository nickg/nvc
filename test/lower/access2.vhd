package access2 is
    type bv_ptr is access bit_vector;
    function get_fresh(b : bit_vector) return bv_ptr;
end package;

package body access2 is

    function get_fresh(b : bit_vector) return bv_ptr is
    begin
        return new bit_vector(b'range);
    end function;

end package body;
