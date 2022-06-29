-- From UVVM uvvm_util/src/methods_pkg.vhd
-- License: Apache 2.0
--
package p1 is

    type t_byte_endianness is (LOWER_BYTE_LEFT, FIRST_BYTE_LEFT, LOWER_BYTE_RIGHT, FIRST_BYTE_RIGHT) ;

    type t_slv_array is array(natural range <>) of bit_vector ;

    subtype t_byte_array is t_slv_array(open)(7 downto 0) ;

    function convert_byte_array_to_slv_array(
    constant byte_array      : t_byte_array;
    constant bytes_in_word   : natural;
    constant byte_endianness : t_byte_endianness := LOWER_BYTE_LEFT
    ) return t_slv_array;

end package ;

package body p1 is


  -- Converts a t_byte_array (any direction) to a t_slv_array (same direction)
  function convert_byte_array_to_slv_array(
    constant byte_array      : t_byte_array;
    constant bytes_in_word   : natural;
    constant byte_endianness : t_byte_endianness := LOWER_BYTE_LEFT
    ) return t_slv_array is
    constant c_num_words        : integer := byte_array'length/bytes_in_word;
    variable v_ascending_array  : t_slv_array(0 to c_num_words-1)((8*bytes_in_word)-1 downto 0);
    variable v_descending_array : t_slv_array(c_num_words-1 downto 0)((8*bytes_in_word)-1 downto 0);
    variable v_byte_idx         : integer := 0;
  begin
    for slv_idx in 0 to c_num_words-1 loop
      if (byte_endianness = LOWER_BYTE_LEFT) or (byte_endianness = FIRST_BYTE_LEFT) then
        for byte_in_word in bytes_in_word downto 1 loop
          v_ascending_array(slv_idx)((8*byte_in_word)-1 downto (byte_in_word-1)*8)  := byte_array(v_byte_idx);
          v_descending_array(slv_idx)((8*byte_in_word)-1 downto (byte_in_word-1)*8) := byte_array(v_byte_idx);
          v_byte_idx                                                                := v_byte_idx + 1;
        end loop;
      else                              -- LOWER_BYTE_RIGHT or FIRST_BYTE_RIGHT
        for byte_in_word in 1 to bytes_in_word loop
          v_ascending_array(slv_idx)((8*byte_in_word)-1 downto (byte_in_word-1)*8)  := byte_array(v_byte_idx);
          v_descending_array(slv_idx)((8*byte_in_word)-1 downto (byte_in_word-1)*8) := byte_array(v_byte_idx);
          v_byte_idx                                                                := v_byte_idx + 1;
        end loop;
      end if;
    end loop;

    if byte_array'ascending then
      return v_ascending_array;
    else                                -- byte array is descending
      return v_descending_array;
    end if;
  end function;

end package body ;

entity array9 is end entity ;

use work.p1.all;

architecture arch of array9 is

    signal s : t_byte_array(0 to 3) := ( X"44", X"55", X"66", X"77" );
begin

    process
    begin
        report to_string(convert_byte_array_to_slv_array(s, 1)(0));
        assert convert_byte_array_to_slv_array(s, 1) = s;
        assert convert_byte_array_to_slv_array(s, 2) = (X"4455", X"6677");
        assert convert_byte_array_to_slv_array(s, 4) = (0 => X"44556677");
        std.env.stop ;
    end process;

end architecture ;
