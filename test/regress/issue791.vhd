entity issue791 is
end entity;

architecture beh of issue791 is

  type t_slv_array is array (natural range <>) of bit_vector;

  procedure read_mem is
    -- This shouldn't be allocated on the stack
    variable v_memory : t_slv_array(0 to (8 * 131072)-1)(7 downto 0) := (others => x"FF");
  begin
    wait for 1 ns;
    v_memory(v_memory'right) := X"ab";
    wait for 1 ns;
    assert v_memory(v_memory'right) = X"ab";
  end procedure;

begin

  p_proc : process
  begin
    read_mem;
    wait;
  end process;
end architecture;
