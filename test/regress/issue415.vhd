library ieee;
use ieee.std_logic_1164.all;

entity bug5 is
  port(
    clk  : in std_logic;
    addr : std_logic_vector
  );
end bug5;

architecture behavioral of bug5 is
  -- This complains with stack trace:
  -- Fatal: signal cannot have unconstrained array type
    signal last_addr_s : std_logic_vector(addr'range);
  -- This is fine
--  signal last_addr : std_logic_vector(addr'high downto addr'low);

begin
  process(clk)
    -- This causes SIGABRT
    variable last_addr : std_logic_vector(addr'range);
    -- This is fine
--    variable last_addr : std_logic_vector(addr'high downto addr'low);
    type int_arr is array (integer range <>) of integer;
    type chunk_item is record
      memory : boolean;
      data   : int_arr(0 to 1);
    end record;
    constant chunk_empty : chunk_item := (false, (others => -1));
    type mem_arr is array (integer range <>) of chunk_item;

    -- The line below complains with stack trace:
    -- Fatal: attempt to add to already finished block 0
    variable mem2  : mem_arr(0 to 1) := (others => chunk_empty);
    -- This is fine (including with the assigment below)
--    variable mem2  : mem_arr(0 to 1);
  begin
    if rising_edge(clk) then
      if addr = (addr'range => '0') then
        mem2 := (others => chunk_empty);
      end if;
      last_addr_s <= addr;
    end if;
  end process;

  check: process is
  begin
      wait for 5 ns;
      assert last_addr_s = X"00000000000001";
      wait;
  end process;

end behavioral;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue415 is
end issue415;

architecture behavioral of issue415 is
  signal clk : std_logic := '0';
  signal addr  : std_logic_vector(55 downto 0);
begin

  bug2_i: entity work.bug5
    port map (
      clk => clk,
      addr => addr
    );

  stim: process is
  begin
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
      addr <= X"00000000000001";
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      wait;
  end process;

end behavioral;
