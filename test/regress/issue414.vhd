-- Dynamic version wrote 100 Mword in 492 s (nvc) - about 1 GByte RAM
--   without marking, 468 s
--   and write directly to only first block - no search - 422 s
--   and fix address (0), 328 s
--   and fix data (0), 313 s
--   and std_logic_vector addr, 309 s
--   56 bit address, 411741 blocks
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue414 is
end issue414;

architecture behavioral of issue414 is
  signal clk   : std_logic := '0';
  signal addr  : std_logic_vector(55 downto 0);
  signal wdata : std_logic_vector(31 downto 0);

  subtype uword64 is unsigned(63 downto 0);

  signal running : boolean := true;
begin

  process (clk, running)
  begin
    if running then
      clk <= not clk after 5 ns;
    end if;
  end process;

  process
    procedure wr_data(address_in : std_logic_vector; data : std_logic_vector) is
      -- Without this things work!
      constant address : std_logic_vector(address_in'length - 1 downto 0) := address_in;
    begin
      -- Without this I get, for high enough loop counts (1000000+):
      -- zsh: segmentation fault  nvc -a bug4.vhd -e bug4 -r
      wait until clk = '1';
    end;

    procedure wr_data(address : unsigned; data : unsigned) is
    begin
      wr_data(std_logic_vector(address), std_logic_vector(data));
    end;

    variable addr    : unsigned(55 downto 0) := (others => '0');
    variable slvdata : unsigned(31 downto 0) := (others => '0');
  begin
    -- 1000 is OK
    for n in 0 to 4000 loop
--    for n in 0 to 1000000 loop
      wr_data(addr, slvdata);  -- SIGBUS crash
    end loop;

    assert false report "Test OK" severity warning;
    running <= false;
    wait;
  end process;

end behavioral;
