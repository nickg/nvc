package generic_sb_pkg is

  generic(type t_element;
          function element_match(received_element : t_element;
                                 expected_element : t_element) return boolean;
          function to_string_element(element : t_element) return string);

  type t_generic_sb is protected
  end protected;

end package generic_sb_pkg;

package body generic_sb_pkg is
  type t_generic_sb is protected body
  end protected body;
end package body;

-------------------------------------------------------------------------------

entity issue858 is
end entity;

architecture tb of issue858 is
  package slv_sb_pkg is new work.generic_sb_pkg
    generic map(t_element         => bit_vector(7 downto 0),
                element_match     => std_match,  -- Error
                to_string_element => to_string);

  use slv_sb_pkg.all;

  shared variable sb_under_test : slv_sb_pkg.t_generic_sb;
begin
end architecture;
