package test_pkg is
  type t_type is (ONE, TWO, THREE);
  type t_phys is range 1 to 1000000
      units
          thing;
          kilothing = 1000 thing;
      end units;
  subtype t_sub is t_type range TWO to THREE;
end package test_pkg;

-------------------------------------------------------------------------------

entity test is
end entity;

architecture beh of test is
  alias t_type is work.test_pkg.t_type;
  alias t_phys is work.test_pkg.t_phys;
  alias t_sub is work.test_pkg.t_sub;
begin

  p_proc : process
    constant const : t_type := ONE;
    constant const2 : t_phys := 5 kilothing;
  begin
    report to_string(const);            -- OK
    assert const = ONE;                 -- OK
    report to_string(const2);           -- OK
    assert const2 * 2.0 = const2;       -- OK
    wait;
  end process;
end architecture;
