package fifo_generic_pkg is
  generic (type t_element_type);

  type t_fifo_item;
  type t_fifo_item_ptr is access t_fifo_item;
  type t_fifo_item is record
        data    : t_element_type;
        nxt_rec : t_fifo_item_ptr;
  end record;
end package fifo_generic_pkg;

entity test_1 is
end entity;

architecture tb of test_1 is
  package data_queue_pkg is new work.fifo_generic_pkg
    generic map (t_element_type => bit_vector(63 downto 0));
  use data_queue_pkg.all;
begin
end architecture;

entity test is
end entity;

architecture tb of test is
begin
  i_test_1_0 : entity work.test_1;
  i_test_1_1 : entity work.test_1;
end architecture;
