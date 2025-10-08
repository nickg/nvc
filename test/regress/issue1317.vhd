package test3_pkg is

  type t_bus_address_write is record
    valid   : bit;
    address : bit_vector;
  end record;

  type t_bus_write is record
    valid   : bit;
    data    : bit_vector;
  end record;

  type t_bus_mosi is record
    address_write_ch : t_bus_address_write;
    write_ch         : t_bus_write;
  end record;

  subtype t_bus_32_mosi is t_bus_mosi(
    address_write_ch(
      address(31 downto 0)
    ),
    write_ch(
      data(31 downto 0)
    )
  );

end package;

-------------------------------------------------------------------------------

use work.test3_pkg.all;

entity issue1317 is
begin
end entity;

architecture Rtl of issue1317 is

  signal bus_master_s : t_bus_32_mosi;

begin

  process_data_inst: block
    port ( bus_master_p : in t_bus_mosi );
    port map (
      bus_master_p.address_write_ch => bus_master_s.address_write_ch,
      bus_master_p.write_ch         => bus_master_s.write_ch );
  begin

    process is
    begin
      assert bus_master_p.write_ch.data'length = 32;
      assert bus_master_p.address_write_ch.address'length = 32;
      wait;
    end process;
  end block;

end architecture;
