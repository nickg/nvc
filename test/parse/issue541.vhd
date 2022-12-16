entity test is
end entity;

architecture beh of test is
  type t_integer_array  is array (natural range <>) of integer;

    procedure check_val(
      constant values         : in    t_integer_array;
      constant name           : in    string := ""
      ) is
    begin
    end procedure;

    procedure check_val(
      constant values_1       : in    integer_vector;
      constant values_2       : in    integer_vector;
      constant name           : in    string := ""
      ) is
    begin
    end procedure;
begin

  p_proc : process
  begin
    check_val(((0 => 1),(0 => 200),(0 => 300)), name => "bin_");
    wait;
  end process;
end architecture;
