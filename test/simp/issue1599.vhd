entity cctrl5nv is
  generic (
    riscv_mmu : integer := 1
    );
end;

architecture rtl of cctrl5nv is

  function pa_msb return integer is
  begin
    return riscv_mmu + 32;
  end;

  constant ppn : bit_vector(pa_msb downto 12) := (others => '0');

  -- Never called; its mere presence triggers the crash.
  function pte_cached return bit is
    variable paddr : bit_vector(ppn'length - 1 downto 0);
  begin
    return paddr(0);
  end;

  signal s : bit_vector(pa_msb + 1 downto 0);
begin
end;

entity top_repro is
end;

architecture tb of top_repro is
begin
  u0 : entity work.cctrl5nv generic map (riscv_mmu => 1);
end;
