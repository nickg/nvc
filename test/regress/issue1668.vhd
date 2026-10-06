package issue1668_pkg is
  type vector_t is array (natural range <>) of bit;

  type left_t is record
    value : vector_t;
  end record;

  type right_t is record
    value : vector_t;
  end record;

  type item_t is record
    left  : left_t;
    right : right_t;
  end record;

  type item_array_t is array (natural range <>) of item_t;
  subtype right32_array_t is item_array_t(open)(right(value(31 downto 0)));
end package;

use work.issue1668_pkg.all;

entity issue1668 is
end entity;

architecture test of issue1668 is
  signal s : right32_array_t(0 to 1)(left(value(23 downto 0)));
begin
  process is
  begin
    assert s'length = 2;
    assert s(0).left.value'length = 24;
    assert s(0).right.value'length = 32;
    report "PASSED" severity note;
    wait;
  end process;
end architecture;
