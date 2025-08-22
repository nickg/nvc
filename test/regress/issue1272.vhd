entity test is
  port (
    i_data : in  integer_vector
  );
end entity;

architecture rtl of test is

  type t_real_record is record
    element : real; -- The crash still appears if this is `integer` rather than `real`.
  end record;

  -- Note - using `i_data'range` or `i_data'high downto i_data'low` here instead of `i_data'high - 1 downto i_data'low`
  -- seems to work fine. Using types other than `t_real_record` also seem to work fine.
  type t_real_record_array is array (i_data'high - 1 downto i_data'low) of t_real_record;

  signal my_sig : t_real_record_array;

begin

  my_sig <= (others => (others => 0.0));

  proc_model : process

    variable v_my_var : t_real_record_array;

  begin
    v_my_var := my_sig;
    assert v_my_var = (9 downto 0 => (element => real'left));
    wait for 0 ns;
    v_my_var := my_sig;
    assert v_my_var = (9 downto 0 => (element => 0.0));
    wait;
  end process;

end architecture;

-------------------------------------------------------------------------------

entity issue1272 is
end entity;

architecture tb of issue1272 is
begin

  cmp_test : entity work.test(rtl)
    port map (
      i_data => (10 downto 0 => 0)
    );

end architecture;

