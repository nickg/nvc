entity issue908 is
end entity;

architecture sim of issue908 is

  type t_nat_vec is array (natural range <>) of natural;

  function count_drivers (inputs : t_nat_vec) return natural is
  begin
    return inputs'length;
  end function;

  subtype t_rnat is count_drivers natural;

  type t_queue_dir is (H2C, C2H, CMP);
  type t_queue_selects is array (t_queue_dir) of t_rnat;

  signal s_queue_selects : t_queue_selects;

begin

  process (all) is
  begin
    for page in 0 to 2 loop  -- H2C, C2H, CMP in separate memory pages
      if (page = 2) then
        -- The longest static prefix of this expression is s_queue_selects
        s_queue_selects(t_queue_dir'val(page-1)) <= 0;
      end if;
    end loop;
  end process;

  check: process is
  begin
    assert s_queue_selects = (H2C to CMP => 1);
    wait;
  end process;

end architecture sim;
