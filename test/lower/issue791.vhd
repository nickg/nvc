entity issue791 is
end entity;

architecture beh of issue791 is

  type t_slv_array is array (natural range <>) of bit_vector;

  procedure read_mem is
    variable v_memory : t_slv_array(0 to 131072-1)(7 downto 0) := (others => x"FF");
    variable v_null   : t_slv_array(0 to 131072-1)(7 to 0) := (others => x"");
    variable v_memory2 : t_slv_array(0 to 7)(7 downto 0) := (others => x"FE");
  begin
  end procedure;

begin

  p_proc : process
  begin
    read_mem;
    wait;
  end process;
end architecture;
