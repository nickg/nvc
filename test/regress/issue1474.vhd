package axi_stream_pkg is
    -- unconstrained record fields
    type axis_m2s_t is record
        tvalid : bit;
        tlast  : bit;
        tdata  : bit_vector;
        tkeep  : bit_vector;
        tstrb  : bit_vector;
        tid    : bit_vector;
        tdest  : bit_vector;
        tuser  : bit_vector;
    end record;

    type axis_s2m_t is record
        tready : bit;
    end record;

    -- Tx/Rx aliases
    subtype tx_axis_out_t is axis_m2s_t;
    subtype tx_axis_in_t  is axis_s2m_t;
    subtype rx_axis_in_t  is axis_m2s_t;
    subtype rx_axis_out_t is axis_s2m_t;
end package;

package body axi_stream_pkg is
end package body;


use work.axi_stream_pkg;

package axi_stream_generic_pkg is
    generic (
        G_NUM_BYTES: positive := 1;
        G_ID_WIDTH: natural := 0;
        G_DEST_WIDTH: natural := 0;
        G_USER_WIDTH: natural := 0
    );

    constant C_NUM_BYTES:  positive := G_NUM_BYTES;
    constant C_ID_WIDTH:   positive := maximum(1, G_ID_WIDTH);
    constant C_DEST_WIDTH: positive := maximum(1, G_DEST_WIDTH);
    constant C_USER_WIDTH: positive := maximum(1, G_USER_WIDTH);

    -- constraint lengths of record fields
    subtype axis_m2s_t is axi_stream_pkg.axis_m2s_t(
        tdata(C_NUM_BYTES*8-1 downto 0),
        tkeep(C_NUM_BYTES-1 downto 0),
        tstrb(C_NUM_BYTES-1 downto 0),
        tid  (C_ID_WIDTH-1 downto 0),
        tdest(C_DEST_WIDTH-1 downto 0),
        tuser(C_USER_WIDTH-1 downto 0)
    );

    subtype axis_s2m_t is axi_stream_pkg.axis_s2m_t;

    -- Tx/Rx Aliases
    subtype tx_axis_out_t is axis_m2s_t;
    subtype tx_axis_in_t  is axis_s2m_t;
    subtype rx_axis_in_t  is axis_m2s_t;
    subtype rx_axis_out_t is axis_s2m_t;
end package;

package body axi_stream_generic_pkg is
end package body;

-------------------------------------------------------------------------------

package device_specific_package is
    -- instantiated within a package
    package device_axis_if is new work.axi_stream_generic_pkg
        generic map (
            G_NUM_BYTES => 2
        );
end package;

-------------------------------------------------------------------------------

use work.device_specific_package.all;
use std.env.finish;

entity issue1474 is
    port (
        dout : out device_axis_if.axis_m2s_t
    );
end entity issue1474;

architecture rtl of issue1474 is
begin
    process begin
        assert dout.tdata'length = 16;
        finish;
    end process;
end architecture;
