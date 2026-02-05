package test_pkg is
  type     t_slv_array   is array (natural range <>) of bit_vector;
  subtype  t_word_array  is t_slv_array(open)(t_word'range);
  constant C_NULL_DATA : t_word_array(0 to -1) := (others => (foo => '0'));
end package;

--------------------------------------------------------------------------------

PACKAGE pkg IS
  CONSTANT c : INTEGER := 16#_.FF#E0;  -- Error
END PACKAGE;

--------------------------------------------------------------------------------

package p is
  pure bad1 is new gen_clock;
end package;
