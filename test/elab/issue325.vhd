-- test.vhd
entity test is
  port (
    clk   : in  bit;
    rst   : in  bit;
    val_o : out bit_vector(15 downto 0)
  );
end entity test;

architecture rtl of test is
begin  -- architecture rtl
end architecture rtl;

-- test_tb.vhd
entity test_tb is
end entity test_tb;

architecture tb of test_tb is
  -- component ports
  signal clk      : bit := '1';
  signal rst      : bit := '1';
  signal val_o    : bit_vector(15 downto 0);
  --
  signal sim_done : boolean := false;
begin  -- architecture tb
  -- component instantiation
  DUT: entity work.test                 -- Fails if library is not called "work"
    port map (
      clk   => clk,
      rst   => rst,
      val_o => val_o);
end architecture tb;
