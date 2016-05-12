package cpu2j0_pack is
   type cpu_debug_o_t is record
      ack  : bit;
      d    : bit_vector(31 downto 0);
      rdy  : bit;
   end record;

   constant bits_exp : natural := 5;
   constant bits     : natural := 2**bits_exp;

   type bus_val_t is record
       en : bit;
       d  : bit_vector(bits-1 downto 0);
   end record;

   constant BUS_VAL_RESET : bus_val_t := ('0', (others => '0'));

   type ybus_val_pipeline_t is array (2 downto 0) of bus_val_t;

   type datapath_reg_t is record
       debug_o    : cpu_debug_o_t;
       ybus_override : ybus_val_pipeline_t;
   end record;

   constant DATAPATH_RESET : datapath_reg_t := (
       debug_o => (ack => '0', d => (others => '0'), rdy => '0'),
       ybus_override => (others => BUS_VAL_RESET) );

end package;

-------------------------------------------------------------------------------

entity jcore5 is
end entity;

use work.cpu2j0_pack.all;

architecture test of jcore5 is
    signal x : datapath_reg_t := DATAPATH_RESET;
begin

    process is
    begin
        assert x = (
       debug_o => (ack => '0', d => (others => '0'), rdy => '0'),
       ybus_override => (others => BUS_VAL_RESET) );
        wait;
    end process;

end architecture;
