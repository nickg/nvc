package model_pkg is
  type t_axilite_rd_slave_in_if is record
    araddr  : bit_vector;
    arvalid : bit;
    rready  : bit;
  end record;

end package model_pkg;

use work.model_pkg.all;

entity model is
  port(
    rd_port_in  : in  t_axilite_rd_slave_in_if(araddr(31 downto 0))
  );
end entity model;

architecture beh of model is
begin
end architecture beh;

entity issue859 is
end entity;

architecture tb of issue859 is

  -- AXI-Lite Interface signals
  type t_axilite_write_address_channel is record
    --DUT inputs
    awaddr  : bit_vector;
    awvalid : bit;
    awprot  : bit_vector(2 downto 0);
    --DUT outputs
    awready : bit;
  end record;

  type t_axilite_write_data_channel is record
    --DUT inputs
    wdata  : bit_vector;
    wstrb  : bit_vector;
    wvalid : bit;
    --DUT outputs
    wready : bit;
  end record;

  type t_axilite_write_response_channel is record
    --DUT inputs
    bready : bit;
    --DUT outputs
    bresp  : bit_vector(1 downto 0);
    bvalid : bit;
  end record;

  type t_axilite_read_address_channel is record
    --DUT inputs
    araddr  : bit_vector;
    arvalid : bit;
    arprot  : bit_vector(2 downto 0);
    --DUT outputs
    arready : bit;
  end record;

  type t_axilite_read_data_channel is record
    --DUT inputs
    rready : bit;
    --DUT outputs
    rdata  : bit_vector;
    rresp  : bit_vector(1 downto 0);
    rvalid : bit;
  end record;

  type t_axilite_if is record
    write_address_channel  : t_axilite_write_address_channel;
    write_data_channel     : t_axilite_write_data_channel;
    write_response_channel : t_axilite_write_response_channel;
    read_address_channel   : t_axilite_read_address_channel;
    read_data_channel      : t_axilite_read_data_channel;
  end record;

  constant C_ADDR_WIDTH_2 : natural := 32;
  constant C_DATA_WIDTH_2 : natural := 64;
  signal axilite_if_2 : t_axilite_if(write_address_channel(awaddr(31 downto 0)),
                                     write_data_channel(wdata(63 downto 0),
                                                        wstrb(7 downto 0)),
                                     read_address_channel(araddr(31 downto 0)),
                                     read_data_channel(rdata(63 downto 0)));
begin

  i_model : entity work.model
    port map(
      rd_port_in.araddr   => axilite_if_2.read_address_channel.araddr,
      rd_port_in.arvalid  => axilite_if_2.read_address_channel.arvalid,
      rd_port_in.rready   => axilite_if_2.read_data_channel.rready
    );

  p_main: process
  begin
    wait;
  end process p_main;
end architecture;
