entity issue1347 is end entity;

architecture test of issue1347 is
  type reg_addrs_t is record
    A, B, C, D : integer;
  end record;
  constant REGS : reg_addrs_t := (A => 0, B => 42, C => 3, D => -1024);
  signal sel, sig : integer;
begin
  process (all)
  begin
    case sel is
      when REGS.A => sig <= 1;
      when REGS.B => sig <= 2;
      when REGS.C |
           REGS.D => sig <= 3;
      when OTHERS => sig <= -1;
    end case;
  end process;

  process
  begin
    sel <= -1;
    wait for 1 ns;
    assert sig = -1;

    sel <= 0;
    wait for 1 ns;
    assert sig = 1;
    
    sel <= 42;
    wait for 1 ns;
    assert sig = 2;
    
    sel <= 3;
    wait for 1 ns;
    assert sig = 3;

    sel <= -1024;
    wait for 1 ns;
    assert sig = 3;
    
    wait;
  end process;
end architecture;
