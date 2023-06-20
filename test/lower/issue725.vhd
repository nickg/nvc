package test is
  type t_slv_array is array (natural range <>) of bit_vector;
  subtype t_word              is bit_vector(15 downto 0);
  subtype t_word_array        is t_slv_array(open)(t_word'range);
  subtype t_addr              is natural range 1 to 30;
  type t_data is array (t_addr) of t_word_array(0 to 31);  -- OK
end package test;
