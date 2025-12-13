entity module_2 is
  generic (
    GC_DATA_WIDTH : positive := 16
  );
  port(
    data : in bit_vector(GC_DATA_WIDTH-1 downto 0)
  );
end entity module_2;

architecture rtl of module_2 is
begin

    process is
    begin
        assert data = X"0000";
        wait for 0 ns;
        assert data = X"0000";
        wait for 0 ns;
        assert data = X"42ff";
        wait;
    end process;
end architecture rtl;

-------------------------------------------------------------------------------

entity issue1361 is
end entity issue1361;

architecture rtl of issue1361 is
  signal data_a : bit_vector(7 downto 0);
  signal data_b : bit_vector(7 downto 0);
begin
  i_module_2 : entity work.module_2
    generic map(
      GC_DATA_WIDTH => 16
    )
    port map(
      data => data_a & data_b
    );

  data_a <= X"42";
  data_b <= X"ff";
end architecture rtl;
