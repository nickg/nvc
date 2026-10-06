package issue1670_pkg is
  type config_t is record
    width : natural;
  end record;
end package;

use work.issue1670_pkg.all;

entity issue1670 is
  generic (
    config : config_t := (width => 32)
  );
end entity;

architecture test of issue1670 is
  type item_t is record
    value : bit_vector;
  end record;

  type item_array_t is array (natural range <>) of item_t;

  signal items : item_array_t(1 downto 0)(
    value(config.width - 1 downto 0)
  );
begin
  process is
    constant width : natural := items'element.value'length;
  begin
    assert width = config.width;
    report "PASSED" severity note;
    wait;
  end process;
end architecture;
