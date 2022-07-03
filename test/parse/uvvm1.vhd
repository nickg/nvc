-- From UVVM ti_vvc_framework_support_pkg.vhd
-- License: Apache 2.0
--
package uvvm1 is

  type unsigned is array (natural range <>) of bit;

  type t_direction is (TRANSMIT, RECEIVE);

  type t_dut_if_field_config is record
    dut_address           : unsigned;    -- Address of the DUT IF field
    dut_address_increment : integer;     -- Incrementation of the address on each access
    data_width            : positive;    -- Width of the data per transfer
    use_field             : boolean;     -- Used by the HVVC to send/request fields to/from the bridge or ignore them when not applicable
    field_description     : string;      -- Description of the DUT IF field
  end record;

  constant C_DUT_IF_FIELD_CONFIG_DEFAULT : t_dut_if_field_config(dut_address(0 downto 0)) := (
    dut_address                => (others => '0'),
    dut_address_increment      => 0,
    data_width                 => 8,
    use_field                  => true,
    field_description          => "default");

  type t_dut_if_field_config_array is array (natural range <>) of t_dut_if_field_config;

  type t_dut_if_field_config_direction_array is array (t_direction range <>) of t_dut_if_field_config_array;

  constant C_DUT_IF_FIELD_CONFIG_DIRECTION_ARRAY_DEFAULT :
      t_dut_if_field_config_direction_array(t_direction'low to t_direction'high)(0 to 0)(dut_address(0 downto 0), field_description(1 to 7))
      -- OK
      := (others => (others => C_DUT_IF_FIELD_CONFIG_DEFAULT));

end package;
