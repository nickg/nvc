library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test_pkg is
  type t_axi_wr_slave_in_if is record
    awid      : std_logic_vector;
    awaddr    : std_logic_vector;
    awlen     : std_logic_vector(7 downto 0);
    awsize    : std_logic_vector(2 downto 0);
    awburst   : std_logic_vector(1 downto 0);
    awlock    : std_logic;
    awcache   : std_logic_vector(3 downto 0);
    awprot    : std_logic_vector(2 downto 0);
    awqos     : std_logic_vector(3 downto 0);
    awregion  : std_logic_vector(3 downto 0);
    awuser    : std_logic_vector;
    awvalid   : std_logic;
    wdata   : std_logic_vector;
    wstrb   : std_logic_vector;
    wlast   : std_logic;
    wuser   : std_logic_vector;
    wvalid  : std_logic;
    bready  : std_logic;
  end record;
  type t_axi_wr_slave_out_if is record
    awready : std_logic;
    wready  : std_logic;
    bid     : std_logic_vector;
    bresp   : std_logic_vector(1 downto 0);
    buser   : std_logic_vector;
    bvalid  : std_logic;
  end record;
  type t_axi_rd_slave_in_if is record
    arid      : std_logic_vector;
    araddr    : std_logic_vector;
    arlen     : std_logic_vector(7 downto 0);
    arsize    : std_logic_vector(2 downto 0);
    arburst   : std_logic_vector(1 downto 0);
    arlock    : std_logic;
    arcache   : std_logic_vector(3 downto 0);
    arprot    : std_logic_vector(2 downto 0);
    arqos     : std_logic_vector(3 downto 0);
    arregion  : std_logic_vector(3 downto 0);
    aruser    : std_logic_vector;
    arvalid   : std_logic;
    rready  : std_logic;
  end record;
  type t_axi_rd_slave_out_if is record
    arready : std_logic;
    rid     : std_logic_vector;
    rdata   : std_logic_vector;
    rresp   : std_logic_vector(1 downto 0);
    rlast   : std_logic;
    ruser   : std_logic_vector;
    rvalid  : std_logic;
  end record;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test_pkg.all;

entity test2 is
  port (
    wr_port_in     : in  t_axi_wr_slave_in_if;
    wr_port_out    : out t_axi_wr_slave_out_if;
    rd_port_in     : in  t_axi_rd_slave_in_if;
    rd_port_out    : out t_axi_rd_slave_out_if
  );
end entity test2;
architecture beh of test2 is
begin
end architecture beh;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test_pkg.all;

entity issue544 is
end entity;
architecture beh of issue544 is
  type t_axi_write_address_channel is record
    awid      : std_logic_vector;
    awaddr    : std_logic_vector;
    awlen     : std_logic_vector(7 downto 0);
    awsize    : std_logic_vector(2 downto 0);
    awburst   : std_logic_vector(1 downto 0);
    awlock    : std_logic;
    awcache   : std_logic_vector(3 downto 0);
    awprot    : std_logic_vector(2 downto 0);
    awqos     : std_logic_vector(3 downto 0);
    awregion  : std_logic_vector(3 downto 0);
    awuser    : std_logic_vector;
    awvalid   : std_logic;
    awready   : std_logic;
  end record;

  type t_axi_write_data_channel is record
    wdata   : std_logic_vector;
    wstrb   : std_logic_vector;
    wlast   : std_logic;
    wuser   : std_logic_vector;
    wvalid  : std_logic;
    wready  : std_logic;
  end record;

  type t_axi_write_response_channel is record
    bid     : std_logic_vector;
    bresp   : std_logic_vector(1 downto 0);
    buser   : std_logic_vector;
    bvalid  : std_logic;
    bready  : std_logic;
  end record;

  type t_axi_read_address_channel is record
    -- Source: Master
    arid      : std_logic_vector;
    araddr    : std_logic_vector;
    arlen     : std_logic_vector(7 downto 0);
    arsize    : std_logic_vector(2 downto 0);
    arburst   : std_logic_vector(1 downto 0);
    arlock    : std_logic;
    arcache   : std_logic_vector(3 downto 0);
    arprot    : std_logic_vector(2 downto 0);
    arqos     : std_logic_vector(3 downto 0);
    arregion  : std_logic_vector(3 downto 0);
    aruser    : std_logic_vector;
    arvalid   : std_logic;
    -- Source: Slave
    arready   : std_logic;
  end record;

  type t_axi_read_data_channel is record
    -- Source: Slave
    rid     : std_logic_vector;
    rdata   : std_logic_vector;
    rresp   : std_logic_vector(1 downto 0);
    rlast   : std_logic;
    ruser   : std_logic_vector;
    rvalid  : std_logic;
    -- Source: Master
    rready  : std_logic;
  end record;

  type t_axi_if is record
    write_address_channel  : t_axi_write_address_channel;
    write_data_channel     : t_axi_write_data_channel;
    write_response_channel : t_axi_write_response_channel;
    read_address_channel   : t_axi_read_address_channel;
    read_data_channel      : t_axi_read_data_channel;
  end record;

  signal axi_if : t_axi_if( write_address_channel( awid(   8 -1 downto 0),
                                                   awaddr( 32-1 downto 0),
                                                   awuser( 8 -1 downto 0)),
                            write_data_channel(    wdata(  32-1 downto 0),
                                                   wstrb(  4 -1 downto 0),
                                                   wuser(  8 -1 downto 0)),
                            write_response_channel(bid(    8 -1 downto 0),
                                                   buser(  8 -1 downto 0)),
                            read_address_channel(  arid(   8 -1 downto 0),
                                                   araddr( 32-1 downto 0),
                                                   aruser( 8-1 downto 0)),
                            read_data_channel(     rid(    8-1 downto 0),
                                                   rdata(  32-1 downto 0),
                                                   ruser(  8-1 downto 0)));
begin
  i_test : entity work.test2
    port map (
      wr_port_in.awid     => axi_if.write_address_channel.awid,
      wr_port_in.awaddr   => axi_if.write_address_channel.awaddr,
      wr_port_in.awlen    => axi_if.write_address_channel.awlen,
      wr_port_in.awsize   => axi_if.write_address_channel.awsize,
      wr_port_in.awburst  => axi_if.write_address_channel.awburst,
      wr_port_in.awlock   => axi_if.write_address_channel.awlock,
      wr_port_in.awcache  => axi_if.write_address_channel.awcache,
      wr_port_in.awprot   => axi_if.write_address_channel.awprot,
      wr_port_in.awqos    => axi_if.write_address_channel.awqos,
      wr_port_in.awregion => axi_if.write_address_channel.awregion,
      wr_port_in.awuser   => axi_if.write_address_channel.awuser,
      wr_port_in.awvalid  => axi_if.write_address_channel.awvalid,
      wr_port_in.wdata    => axi_if.write_data_channel.wdata,
      wr_port_in.wstrb    => axi_if.write_data_channel.wstrb,
      wr_port_in.wlast    => axi_if.write_data_channel.wlast,
      wr_port_in.wuser    => axi_if.write_data_channel.wuser,
      wr_port_in.wvalid   => axi_if.write_data_channel.wvalid,
      wr_port_in.bready   => axi_if.write_response_channel.bready,
      rd_port_in.arid     => axi_if.read_address_channel.arid,
      rd_port_in.araddr   => axi_if.read_address_channel.araddr,
      rd_port_in.arlen    => axi_if.read_address_channel.arlen,
      rd_port_in.arsize   => axi_if.read_address_channel.arsize,
      rd_port_in.arburst  => axi_if.read_address_channel.arburst,
      rd_port_in.arlock   => axi_if.read_address_channel.arlock,
      rd_port_in.arcache  => axi_if.read_address_channel.arcache,
      rd_port_in.arprot   => axi_if.read_address_channel.arprot,
      rd_port_in.arqos    => axi_if.read_address_channel.arqos,
      rd_port_in.arregion => axi_if.read_address_channel.arregion,
      rd_port_in.aruser   => axi_if.read_address_channel.aruser,
      rd_port_in.arvalid  => axi_if.read_address_channel.arvalid,
      rd_port_in.rready   => axi_if.read_data_channel.rready,
      wr_port_out.awready => axi_if.write_address_channel.awready,
      wr_port_out.wready  => axi_if.write_data_channel.wready,
      wr_port_out.bid     => axi_if.write_response_channel.bid,
      wr_port_out.bresp   => axi_if.write_response_channel.bresp,
      wr_port_out.buser   => axi_if.write_response_channel.buser,
      wr_port_out.bvalid  => axi_if.write_response_channel.bvalid,
      rd_port_out.arready => axi_if.read_address_channel.arready,
      rd_port_out.rid     => axi_if.read_data_channel.rid,
      rd_port_out.rdata   => axi_if.read_data_channel.rdata,
      rd_port_out.rresp   => axi_if.read_data_channel.rresp,
      rd_port_out.rlast   => axi_if.read_data_channel.rlast,
      rd_port_out.ruser   => axi_if.read_data_channel.ruser,
      rd_port_out.rvalid  => axi_if.read_data_channel.rvalid
    );

  process
  begin
    report "OK";
    wait;
  end process;

end architecture beh;
