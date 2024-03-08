package pack is
    type t_sbi_if is record
        addr  : bit_vector;
        wdata : bit_vector;
        rdata : bit_vector;
    end record;
end package;

use work.pack.all;

entity ethernet_th is
  generic(
    GC_SBI_ADDR_WIDTH : positive := 8;
    GC_SBI_DATA_WIDTH : positive := 8
  );
end entity ethernet_th;

architecture struct_sbi of ethernet_th is
  signal clk       : bit;
  signal i1_sbi_if : t_sbi_if(addr(GC_SBI_ADDR_WIDTH - 1 downto 0), wdata(GC_SBI_DATA_WIDTH - 1 downto 0), rdata(GC_SBI_DATA_WIDTH - 1 downto 0));
begin
end architecture struct_sbi;

use work.pack.all;

entity test is
  generic(
    GC_DATA_WIDTH : positive := 8);
end entity;

architecture tb of test is
  constant C_ADDR_WIDTH : positive := 8;

  alias clk is << signal .test.i_test_harness.clk : bit >>;  -- Error
  alias i1_sbi_if is << signal .test.i_test_harness.i1_sbi_if :
      t_sbi_if(addr(C_ADDR_WIDTH - 1 downto 0), wdata(GC_DATA_WIDTH - 1 downto 0), rdata(GC_DATA_WIDTH - 1 downto 0)) >>; -- Error
begin

  i_test_harness : entity work.ethernet_th(struct_sbi)
    generic map(
      GC_SBI_ADDR_WIDTH => C_ADDR_WIDTH,
      GC_SBI_DATA_WIDTH => GC_DATA_WIDTH
    );

  p_main: process
  begin
    wait;
  end process p_main;
end architecture;
