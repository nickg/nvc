entity bug is end entity;

architecture test of bug is
  type reg_addrs_t is record
    A, B, C, D, E, F : integer range 0 to 5;
  end record;
  constant REGS : reg_addrs_t := (A => 0, B => 1, C => 2, D => 3, E => 4, F => 5);
  signal sel, sig : integer range 0 to 5;
begin
  process (all)
  begin
    case sel is
      when REGS.A | REGS.B => sig <= 0;
      when integer(REGS.C) to integer(REGS.D) => sig <= 1;
      when REGS.E to REGS.F => sig <= 2;
    end case;
  end process;

  process
  begin
    sel <= 0;
    wait for 1 ns;
    assert sig = 0;

    sel <= 1;
    wait for 1 ns;
    assert sig = 0;

    sel <= 2;
    wait for 1 ns;
    assert sig = 1;

    sel <= 3;
    wait for 1 ns;
    assert sig = 1;

    sel <= 4;
    wait for 1 ns;
    assert sig = 2;

    sel <= 5;
    wait for 1 ns;
    assert sig = 2;

    wait;
  end process;
end architecture;
