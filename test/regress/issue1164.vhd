entity issue1164 is
end entity;

architecture arch of issue1164 is
  type rec_part is record
    a : bit;
  end record;

  type rec_part_array is array (natural range <>) of rec_part;

  type t_rec is record
    rec_part_a : rec_part;
    arr : rec_part_array(1 to 2);
    b : bit;
  end record t_rec;

  signal rec_sig : t_rec;
begin


  detect : process is
    variable var_time : time;
  begin
    var_time := rec_sig'last_event;
    assert var_time = time'high;
    var_time := rec_sig.b'last_event; -- okay
    assert var_time = time'high;
    var_time := rec_sig.rec_part_a'last_event; -- not okay
    assert var_time = time'high;
    var_time := rec_sig.rec_part_a'delayed(0 ns)'last_event; -- not okay
    assert var_time = time'high;
    rec_sig.rec_part_a.a <= '1';
    wait for 1 ns;
    assert rec_sig'last_event = 1 ns;
    assert rec_sig.rec_part_a'last_event = 1 ns;
    assert rec_sig.rec_part_a'delayed(2 ns)'last_event = time'high;
    assert rec_sig'last_active = 1 ns;
    wait;
  end process;

end architecture arch;
