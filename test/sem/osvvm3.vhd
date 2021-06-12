package my_logic is
    type unsigned is array (natural range <>) of bit;
    type signed is array (natural range <>) of bit;

    function to_integer(x : unsigned) return integer;
    function to_integer(x : signed) return integer;
end package;

use work.my_logic.all;

package codec_builder_pkg is
  function from_byte_array (
    constant byte_array : string)
    return bit_vector;

  constant integer_code_length : positive := 4;

  procedure decode (
    constant code   :       string;
    variable index  : inout positive;
    variable result : out   work.my_logic.unsigned);
  procedure decode (
    constant code   :       string;
    variable index  : inout positive;
    variable result : out   work.my_logic.signed);
end package codec_builder_pkg;

package body codec_builder_pkg is
  procedure decode (
    constant code   :       string;
    variable index  : inout positive;
    variable result : out   integer) is
  begin
    result := to_integer(work.my_logic.signed(from_byte_array(code(index to index + integer_code_length - 1))));
    index  := index + integer_code_length;
  end procedure decode;

  procedure decode (
    constant code   :       string;
    variable index  : inout positive;
    variable result : out   work.my_logic.unsigned) is
    variable result_bv : bit_vector(result'range);
  begin
    result := work.my_logic.unsigned(result_bv);
  end;

  procedure decode (
    constant code   :       string;
    variable index  : inout positive;
    variable result : out   work.my_logic.signed) is
    variable result_bv : bit_vector(result'range);
  begin
    result := work.my_logic.signed(result_bv);
  end;

end package body codec_builder_pkg;
