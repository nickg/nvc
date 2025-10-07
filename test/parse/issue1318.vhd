entity test4_tb is
begin

end entity test4_tb;

architecture Rtl of test4_tb is

  type t_bus_address_write is record
    valid   : bit;
    address : bit_vector;
  end record;

  type t_bus_array is array (integer range <>) of t_bus_address_write;

  signal bus_array : t_bus_array(2 downto 0)(
    address(31 downto 0)
  );

  constant const_using_bus_array : bit_vector(bus_array'element.address'range) := (others => '0');

begin

end architecture;
