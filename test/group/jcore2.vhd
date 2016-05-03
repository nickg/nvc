entity jcore2 is
end entity;

architecture test of jcore2 is

constant bits_exp : natural := 5;
constant bits     : natural := 2**bits_exp;

type bus_val_t is record
  en : bit;
  d  : bit_vector(bits-1 downto 0);
end record;

type ybus_val_pipeline_t is array (2 downto 0) of bus_val_t;

type datapath_reg_t is record
   pc         : bit_vector(bits-1 downto 0);
   ybus_override : ybus_val_pipeline_t;
end record;

    signal this_c : datapath_reg_t;
begin

end architecture;
