library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue412 is
end issue412;

architecture behavioral of issue412 is
  signal clk : std_logic := '0';
  signal running : boolean := true;
begin

  process (clk, running)
  begin
    if running then
      clk <= not clk after 5 ns;
    end if;
  end process;

  process
    -- Overloading the name is not the issue.
    procedure wr_data(data : signed) is
    begin
      -- A delay here seems to be necessary to cause the issue.
      wait until clk = '1';
--      wait for 10 ns;
    end;

    -- Calling from this function to the next seems
    -- to be required for the crash.
    procedure wr_data(data : integer) is
    begin
      wr_data(to_signed(data, 32));
    end;

    variable data : signed(31 downto 0);
  begin
    -- Loop to 2000 works with line A below.
--    for n in 1 to 2000 loop
    -- Loop to 3000 does not work with line A below.
    for n in 1 to 3000 loop
    -- Loop to 3000000 works fine with lines B below.
--    for n in 1 to 3000000 loop
      wr_data(n);                -- A
--      data := to_signed(n, 32);  -- B
--      wr_data(data);             -- B
    end loop;

    assert false report "Test OK" severity note;
    running <= false;
    wait;
  end process;

end behavioral;
