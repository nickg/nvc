entity issue185 is
end entity;

architecture a of issue185 is
  type record_t is record
    field : natural;
  end record;

  procedure proc_rec(constant value : in record_t := (field => 0)) is
  begin
    report integer'image(value.field);
    wait for 0 ns;
    report integer'image(value.field);
  end procedure;

  procedure proc_integer(constant value : in integer := 0) is
  begin
    report integer'image(value);
    wait for 0 ns;
    report integer'image(value);
  end procedure;

  procedure proc_string(constant value : in string := "hello") is
  begin
    report value;
    wait for 0 ns;
    report value;
  end procedure;

  procedure proc_bit_vector(constant value : in bit_vector := "0") is
  begin
    report bit'image(value(0));
    wait for 0 ns;
    report bit'image(value(0));
  end procedure;

begin
  main : process
  begin
    report "record parameter does not work";
    proc_rec;
    proc_rec(value => (field => 1));
    report "integer parameter works";
    proc_integer;
    proc_integer(value => 1);
    report "string parameter works";
    proc_string;
    proc_string(value => "foobar");
    report "bit_vector parameter works";
    proc_bit_vector;
    proc_bit_vector(value => "1");
    wait;
  end process;
end architecture;
