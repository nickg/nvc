package test_pkg is
  generic (type t_element;
           function to_string_element(element : t_element) return string
           );
end package;

entity issue520 is
end entity issue520;
architecture beh of issue520 is
  type t_record is record
    address : bit_vector(7 downto 0);
    data    : bit_vector(7 downto 0);
  end record t_record;

  function record_to_string(
    constant rec_data : t_record
  ) return string is
  begin
    return "address: " & to_string(rec_data.address) & ", data: " & to_string(rec_data.data);
  end function record_to_string;

  package record_pkg is new work.test_pkg
  generic map (t_element         => t_record,
               to_string_element => record_to_string);

  use record_pkg.all;

begin
  process
    variable v_input : t_record := (x"AA", x"BB");
  begin
    report "Val is " & to_string_element(v_input) severity note;
      wait for 1 ns;
      assert to_string_element(v_input) = "address: 10101010, data: 10111011";
    wait;
  end process;

end architecture beh;
