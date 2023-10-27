library ieee;
use ieee.math_real.all;

package top_pkg is
  function log2_ceil (
    val: natural)
  return natural;
end package top_pkg;

package body top_pkg is

  function log2_ceil (
    val: natural)
  return natural is
  begin
    return integer(ceil(log2(real(val) + 1.0)));
  end function log2_ceil;
end package body top_pkg;

use work.top_pkg.log2_ceil;

package test_pkg is
  type t_segment_type is (
    VERSION_NUMBER
  );
  constant C_MAX_WORD_LENGTH : natural := 3;
  constant C_MAX_ADDR_WIDTH  : natural := log2_ceil(4095);
  type t_data_segment is record
    word_idx     : bit_vector(C_MAX_ADDR_WIDTH - 1 downto 0);
    word_length  : natural range 0 to C_MAX_WORD_LENGTH;
  end record t_data_segment;

  type     t_data_segment_template is array(t_segment_type) of t_data_segment;
  constant C_SEGMENT_RECORDS : t_data_segment_template := (
    VERSION_NUMBER            => t_data_segment'(
      word_idx                => (others => '0'),
      word_length             => 1)
  );

end package;

use work.test_pkg.all;

entity test is
end entity test;
architecture beh of test is
  signal segment_word        : t_data_segment;
begin

  b_block : block is
   signal calib_version_tmp : bit_vector(C_SEGMENT_RECORDS(VERSION_NUMBER).word_length * 8 - 1 downto 0);
  begin
  process
  begin
    report "OK 2";
    wait;
  end process;
  end block;

end architecture beh;
