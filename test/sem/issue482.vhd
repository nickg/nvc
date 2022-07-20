package test_pkg is
  type t_type   is (ONE, TWO);
  alias t_type2   is t_type;
  constant C_CONST : string := t_type2'image(ONE);
end package;
