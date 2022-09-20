package test_pkg is
  type t_axi_wr_slave_in_if is record
    awid      : bit_vector;
    awaddr    : bit_vector;
    awlen     : bit_vector(7 downto 0);
    awsize    : bit_vector(2 downto 0);
    awburst   : bit_vector(1 downto 0);
    awlock    : bit;
    awcache   : bit_vector(3 downto 0);
    awprot    : bit_vector(2 downto 0);
    awqos     : bit_vector(3 downto 0);
    awregion  : bit_vector(3 downto 0);
    awuser    : bit_vector;
    awvalid   : bit;
    wdata   : bit_vector;
    wstrb   : bit_vector;
    wlast   : bit;
    wuser   : bit_vector;
    wvalid  : bit;
    bready  : bit;
  end record;
end package;

use work.test_pkg.all;

entity test2 is
  port (
    wr_port_in     : in  t_axi_wr_slave_in_if
  );
end entity test2;
architecture beh of test2 is
begin

  process is
  begin
    wait for 1 ns;
    assert wr_port_in.wdata = (31 downto 0 => '1');
    wait;
  end process;

end architecture beh;

use work.test_pkg.all;

entity issue519 is
end entity issue519;
architecture beh of issue519 is
  type t_axi_write_address_channel is record
    awid      : bit_vector;
    awaddr    : bit_vector;
    awlen     : bit_vector(7 downto 0);
    awsize    : bit_vector(2 downto 0);
    awburst   : bit_vector(1 downto 0);
    awlock    : bit;
    awcache   : bit_vector(3 downto 0);
    awprot    : bit_vector(2 downto 0);
    awqos     : bit_vector(3 downto 0);
    awregion  : bit_vector(3 downto 0);
    awuser    : bit_vector;
    awvalid   : bit;
    awready   : bit;
  end record;

  type t_axi_write_data_channel is record
    wdata   : bit_vector;
    wstrb   : bit_vector;
    wlast   : bit;
    wuser   : bit_vector;
    wvalid  : bit;
    wready  : bit;
  end record;

  type t_axi_write_response_channel is record
    bid     : bit_vector;
    bresp   : bit_vector(1 downto 0);
    buser   : bit_vector;
    bvalid  : bit;
    bready  : bit;
  end record;

  type t_axi_if is record
    write_address_channel  : t_axi_write_address_channel;
    write_data_channel     : t_axi_write_data_channel;
    write_response_channel : t_axi_write_response_channel;
  end record;
  signal axi_if : t_axi_if( write_address_channel( awid(   8 -1 downto 0),
                                                   awaddr( 32-1 downto 0),
                                                   awuser( 8 -1 downto 0)),
                            write_data_channel(    wdata(  32-1 downto 0),
                                                   wstrb(  4 -1 downto 0),
                                                   wuser(  8 -1 downto 0)),
                            write_response_channel(bid(    8 -1 downto 0),
                                                   buser(  8 -1 downto 0)));
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
      wr_port_in.bready   => axi_if.write_response_channel.bready
    );

  process
  begin
    axi_if.write_data_channel.wdata <= X"ffffffff";
    wait;
  end process;

end architecture beh;
