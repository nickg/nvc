entity issue204 is
end entity;

architecture a of issue204 is
  type enum_t is (a, b);
begin
  main : process
  begin
    assert enum_t'leftof(b) = a;
    assert enum_t'rightof(a) = b;
    wait;
  end process;
end architecture;
