entity test is
end entity test;
architecture beh of test is

    type unsigned is array (natural range <>) of bit;
begin

  p1: process
    variable v_slv_long_min  : bit_vector(127 downto 0) := (others => '0');
    variable v_slv_long_max  : bit_vector(127 downto 0) := (others => '0');
    type t_unsigned_vector is array (natural range <>) of unsigned;
    type t_range_uns_vec is array (natural range <>) of t_unsigned_vector;

    procedure check_value_long(
      constant range_vec : in t_range_uns_vec) is
    begin
  end procedure;
  begin
    check_value_long((0 => (unsigned(v_slv_long_min), unsigned(v_slv_long_max))));
    wait;
  end process;

end architecture beh;
