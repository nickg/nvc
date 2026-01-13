entity issue1366 is end;

architecture tb of issue1366 is
  type axis_t is record
    data : bit_vector;
  end record;
  type axis_arr_t is array (natural range <>) of axis_t;
  view s_axis_v of axis_t is
    data : out;
  end view;

  signal s_axis : axis_arr_t(0 to 1)(data(1 downto 0));
begin

  b1 : block
    port (s_axis : view (s_axis_v) of axis_arr_t);
    port map (
      s_axis(0) => s_axis(0),
      s_axis(1) => s_axis(1)
    );
  begin
      process is
      begin
          assert s_axis(1).data'length = 2;
          s_axis(1).data <= "10";
          s_axis(0).data <= "01";
          wait;
      end process;
  end block;

  process is
  begin
      wait for 1 ns;
      assert s_axis(1).data = "10";
      assert s_axis(0).data = "01";
      wait;
  end process;

end architecture;
