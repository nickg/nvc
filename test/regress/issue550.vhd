library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test_pkg is
  type t_avalon_mm_if is record
    reset             : std_logic;
    address           : std_logic_vector;
    begintransfer     : std_logic;
    byte_enable       : std_logic_vector;
    chipselect        : std_logic;
    write             : std_logic;
    writedata         : std_logic_vector;
    read              : std_logic;
    lock              : std_logic;
    readdata          : std_logic_vector;
    response          : std_logic_vector(1 downto 0);
    waitrequest       : std_logic;
    readdatavalid     : std_logic;
    irq               : std_logic;
  end record;
  function init_avalon_mm_if_signals(
    addr_width : natural;
    data_width : natural;
    lock_value : std_logic := '0'
    ) return t_avalon_mm_if;
end package;
package body test_pkg is

  function init_avalon_mm_if_signals(
    addr_width : natural;
    data_width : natural;
    lock_value : std_logic := '0'
    ) return t_avalon_mm_if is
    variable result : t_avalon_mm_if(address(addr_width - 1 downto 0),
                                         byte_enable((data_width/8) - 1 downto 0),
                                         writedata(data_width - 1 downto 0),
                                         readdata(data_width-1 downto 0));
  begin
    result.reset            := '0';
    result.address          := (result.address'range => '0');
    result.begintransfer    := '0';
    result.byte_enable      := (result.byte_enable'range => '1');
    result.chipselect       := '0';
    result.write            := '0';
    result.writedata        := (result.writedata'range => '0');
    result.read             := '0';
    result.lock             := lock_value;
    result.readdata         := (result.readdata'range => 'Z');
    result.response         := (result.response'range => 'Z');
    result.waitrequest      := 'Z';
    result.readdatavalid    := 'Z';
    result.irq              := 'Z';
    return result;
  end function;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test_pkg.all;

entity test2 is
  generic (
    GC_ADDR_WIDTH                           : integer range 1 to 64 := 8;          -- Avalon MM address bus
    GC_DATA_WIDTH                           : integer range 1 to 64 := 32         -- Avalon MM data bus
    );
  port (
    clk                           : in std_logic;
    avalon_mm_vvc_master_if       : inout t_avalon_mm_if := init_avalon_mm_if_signals(GC_ADDR_WIDTH, GC_DATA_WIDTH)
  );
begin
end entity test2;
architecture beh of test2 is
  signal avalon_mm_vvc_master_if_pd   : t_avalon_mm_if(address(GC_ADDR_WIDTH-1 downto 0),
                                                      byte_enable((GC_DATA_WIDTH/8)-1 downto 0),
                                                      writedata(GC_DATA_WIDTH-1 downto 0),
                                                      readdata(GC_DATA_WIDTH-1 downto 0)) := avalon_mm_vvc_master_if;
begin
end architecture beh;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test_pkg.all;

entity issue550 is
end entity issue550;
architecture beh of issue550 is
  signal clk : std_logic;
  signal avalon_mm_if   : t_avalon_mm_if(address(31 downto 0), byte_enable(3 downto 0), writedata(31 downto 0), readdata(31 downto 0));
begin
  i_test : entity work.test2
    generic map(
      GC_ADDR_WIDTH   => 32,
      GC_DATA_WIDTH   => 32
    )
    port map (
      clk => clk,
      avalon_mm_vvc_master_if     => avalon_mm_if
    );

  process
  begin
    report "OK";
    wait;
  end process;

end architecture beh;
