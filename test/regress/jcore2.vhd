package data_bus_pkg is
  type data_bus_device_t is (
    DEV_NONE
    ,DEV_PIO
    ,DEV_SPI
    ,DEV_AIC
      ,DEV_UART0
      ,DEV_UART1
      ,DEV_UARTGPS
    ,DEV_SRAM
      ,DEV_DDR
    ,DEV_BL0
    ,DEV_EMAC
      ,DEV_I2C
  );
  type ext_bus_device_t is (
    DEV_BL0,
    DEV_EMAC,
      DEV_I2C,
    DEV_DDR
  );
  type ext_to_int_data_bus_t is array(ext_bus_device_t'left to ext_bus_device_t'right) of data_bus_device_t;
  -- arrays for mapping mcu_lib's data bus and irq ports to the internal versions
  constant ext_to_int_data : ext_to_int_data_bus_t := (
    DEV_BL0 => DEV_BL0,
    DEV_EMAC => DEV_EMAC,
      DEV_I2C => DEV_I2C,
    DEV_DDR => DEV_NONE
  );
end data_bus_pkg;

-------------------------------------------------------------------------------

package monitor_pkg is

  type timeout_t is record
    cnt : integer range 0 to 10;
  end record;

  type cnt_reg_t is record
      a : bit;
      cnt : integer range 0 to 10;
  end record;

constant CNT_REG_RESET : cnt_reg_t := ('0',0);

end package;

-------------------------------------------------------------------------------

use work.monitor_pkg.all;

entity timeout_cnt is
port(
    clk : in bit;
    rst : in bit;
    timeout : out timeout_t
    );

end timeout_cnt;

architecture structure of timeout_cnt is

signal this_c : cnt_reg_t;
signal this_r : cnt_reg_t := CNT_REG_RESET;

begin
counter_r0 : process(clk, rst)
begin
   if rst = '1' then
      this_r <= CNT_REG_RESET;
   elsif clk = '1' and clk'event then
       report integer'image(this_c.cnt);
       this_c.cnt <= this_c.cnt + 1;
      this_r <= this_c;
   end if;
end process;

timeout.cnt <= this_r.cnt;

end structure;


-------------------------------------------------------------------------------

use work.monitor_pkg.all;
use work.data_bus_pkg.all;

entity jcore2 is
end;

architecture behaviour of jcore2 is
  signal clk : bit := '1';
  signal rst : bit := '1';

  signal timeout : timeout_t;
begin

  timeout_cnt_i: entity work.timeout_cnt
      port map(clk => clk, rst => rst,
               timeout => timeout);

  process is
  begin
      wait for 1 ns;
      assert timeout.cnt = 0;
      rst <= '0';
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      assert timeout.cnt = 1;
      wait;
  end process;
end;
