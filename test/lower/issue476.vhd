package test_pkg is
  type    t_slv_array         is array (natural range <>) of bit_vector;
  subtype t_word              is bit_vector(15 downto 0);
  subtype t_word_array        is t_slv_array(open)(t_word'range);
  type t_rec is record
    data               : t_word_array(0 to 1);
  end record;

  constant C_DEFAULT : t_rec := (
    data                => (others => (others => '0'))
    );
end package;
