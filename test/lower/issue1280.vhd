entity test is
  port (
    i_data : in  integer_vector
  );
end entity;

architecture rtl of test is

  type t_real_record is record
    element : real;
    flag : boolean;
  end record;

  type t_real_record_array is array (i_data'range) of t_real_record;

  signal my_sig : t_real_record_array;

begin

  proc_model : process(my_sig)
    variable v_my_var : t_real_record_array;
  begin
    v_my_var := my_sig'delayed(2 ns);
  end process;

end architecture;

-------------------------------------------------------------------------------

entity issue1280 is
end entity;

architecture tb of issue1280 is
begin

  cmp_test : entity work.test(rtl)
    port map (
      i_data => (10 downto 0 => 0)
    );

end architecture;
