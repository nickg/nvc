package interface_function_common is
    function is_unrecognized_primary_command (byte : bit_vector) return boolean;
end interface_function_common;

package body interface_function_common is
    -- Error: does not match specification
    function is_unrecognized_primary_command (byte : bit_vector(7 downto 0)) return boolean is
        variable unsigned_stripped_byte : bit_vector(7 downto 0);
    begin
        unsigned_stripped_byte(6 downto 0) := byte(6 downto 0);
        unsigned_stripped_byte(7) := '0';
        return unsigned_stripped_byte = X"00";
    end is_unrecognized_primary_command;
end package body interface_function_common;
