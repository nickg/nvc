-- Test case from Brian Padalino
--
package p1 is

    type t_byte_endianness is (LOWER_BYTE_LEFT, FIRST_BYTE_LEFT, LOWER_BYTE_RIGHT, FIRST_BYTE_RIGHT) ;

    type t_slv_array is array(natural range <>) of bit_vector ;

    subtype t_byte_array is t_slv_array(open)(7 downto 0) ;

    function convert_byte_array_to_slv(
        constant byte_array : t_byte_array ;
        constant byte_endianness : t_byte_endianness
    ) return bit_vector ;

end package ;

package body p1 is

  -- example taken directly from uvvm:
  --   https://github.com/UVVM/UVVM/blob/92cb1495afa007f74ed79fb9935282196420add0/uvvm_util/src/methods_pkg.vhd#L6801
  function convert_byte_array_to_slv(
    constant byte_array      : t_byte_array;
    constant byte_endianness : t_byte_endianness
    ) return bit_vector is
    constant c_num_bytes        : integer := byte_array'length;
    alias normalized_byte_array : t_byte_array(0 to c_num_bytes-1) is byte_array;
    variable v_slv              : bit_vector(8*c_num_bytes-1 downto 0);
  begin
    assert byte_array'ascending report "byte_array must be ascending" severity error;

    for byte_idx in 0 to c_num_bytes-1 loop
      if (byte_endianness = LOWER_BYTE_LEFT) or (byte_endianness = FIRST_BYTE_LEFT) then
        v_slv(8*(c_num_bytes-byte_idx)-1 downto 8*(c_num_bytes-1-byte_idx)) := normalized_byte_array(byte_idx);
      else                              -- LOWER_BYTE_RIGHT or FIRST_BYTE_RIGHT
        v_slv(8*(byte_idx+1)-1 downto 8*byte_idx) := normalized_byte_array(byte_idx);
      end if;
    end loop;
    return v_slv;
  end function;

end package body ;

entity array8 is end entity ;

use work.p1.all;

architecture arch of array8 is

    signal s : t_byte_array(0 to 2) := ( X"44", X"55", X"66" );
begin

    process
    begin
        assert convert_byte_array_to_slv((X"01", X"02", X"03"), LOWER_BYTE_LEFT) = X"010203";
        assert convert_byte_array_to_slv((X"01", X"02", X"03"), LOWER_BYTE_RIGHT) = X"030201";

        assert convert_byte_array_to_slv(s, LOWER_BYTE_LEFT) = X"445566";
        assert convert_byte_array_to_slv(s, LOWER_BYTE_RIGHT) = X"665544";

        std.env.stop ;
    end process;

end architecture ;
