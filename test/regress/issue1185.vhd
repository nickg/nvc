entity issue1185 is
end entity issue1185;

architecture behav of issue1185 is

  type t_enum is (
    T_SRC_UNKNOWN,
    T_SRC_A_0,
    T_SRC_A_1,
    T_SRC_B_0,
    T_SRC_B_1,
    T_SRC_C
  );

  signal tgt : t_enum;
  signal sel_a, sel_const : boolean;
  signal a,b : t_enum;
begin

  process
  begin
    wait for 1 ns;
    a <= T_SRC_A_1;
    wait for 1 ns;
    a <= T_SRC_A_0;

    wait for 20 ns;
    b <= T_SRC_B_1;
    wait for 1 ns;
    b <= T_SRC_B_0;
    wait for 1 ns;
    b <= T_SRC_B_1;
    wait;
  end process;

  process
  begin
    sel_a <= true;
    wait for 10 ns;
    sel_a <= false;
    sel_const <= false;
    wait for 30 ns;
    sel_const <= true;
    wait for 20 ns;
    wait;
  end process;

  process(all)begin
    tgt <= force       a when sel_a else
                 T_SRC_C when sel_const else
                       b;
    report "Target value is" & t_enum'image(tgt);
  end process;

end architecture;
