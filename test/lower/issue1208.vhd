--------------------------------------------------------------------------------`
-- PACKAGE DECLARATION
--------------------------------------------------------------------------------
package axiLite_pkg is
  -- AxiLite Read Address channel
  type axiLite_rdAddrCh_t is record
    -- Handshake signals
    arvalid : bit;
    arready : bit;
    -- Payload signals
    araddr : bit_vector(7 downto 0);
  end record axiLite_rdAddrCh_t;

  view axiLite_rdAddrCh_Host_v of axiLite_rdAddrCh_t is
    -- Handshake signals
    arvalid : out;
    arready : in;
    -- Payload signals
    araddr  : out;
  end view axiLite_rdAddrCh_Host_v;

  alias axiLite_rdAddrCh_Agent_v is axiLite_rdAddrCh_Host_v'converse;
  ------------------------------------------------------------------------------
  -- AxiLite Read
  type axiLite_t is record
    rdAddrCh : axiLite_rdAddrCh_t;
  end record axiLite_t;

  view axiLite_Host_v of axiLite_t is
    rdAddrCh : view axiLite_rdAddrCh_Host_v;
  end view axiLite_Host_v;

  alias axiLite_Agent_v is axiLite_Host_v'converse;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
end package axiLite_pkg;
--------------------------------------------------------------------------------

use work.axiLite_pkg.all;

entity axiLite_reg is
  port (
    i_axil_clk      : in  bit;  -- Main clock
    i_axil_reset    : in  bit;  -- reset, active high
    -- AXI4-Lite interface
    axil_agent      : view axiLite_Agent_v of axiLite_t
  );
end entity axiLite_reg;

architecture rtl of axiLite_reg is
  signal s_axil_agent_if : axiLite_t;
begin
  -- out
  axil_agent.rdAddrCh.arready      <= s_axil_agent_if.rdAddrCh.arready;
  -- in
  s_axil_agent_if.rdAddrCh.arvalid <= axil_agent.rdAddrCh.arvalid;
  s_axil_agent_if.rdAddrCh.araddr  <= axil_agent.rdAddrCh.araddr;
end architecture rtl;
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
entity issue1208 is
  port (
    -- AXI4Lite
    s_axil_aclk     : in  bit;
    s_axil_areset   : in  bit;
    --
    s_axil_arvalid  : in  bit;
    s_axil_arready  : out bit;
    s_axil_araddr   : in  bit_vector(7 downto 0);
 );
end entity issue1208;

architecture str of issue1208 is

begin  -- start of architecture --
  axiLite_reg_inst : entity work.axiLite_reg(rtl)
    port map (
      i_axil_clk           => s_axil_aclk,
      i_axil_reset         => s_axil_areset,
      --
      axil_agent.rdAddrCh.araddr   => s_axil_araddr,
      axil_agent.rdAddrCh.arvalid  => s_axil_arvalid,
      axil_agent.rdAddrCh.arready  => s_axil_arready
    ); -- end of axiLite_reg_inst
end architecture str;
--------------------------------------------------------------------------------
