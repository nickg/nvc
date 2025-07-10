entity bug is
end entity bug;
architecture tb of bug is
  type arr_t is array(0 to 1) of bit_vector(1 downto 0);
begin
  process is
  begin
    report to_string(arr_t'element'length);
    wait;
  end process;
end architecture;
