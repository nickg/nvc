package generic_queue_pkg is

  generic(type t_generic_element;
          GC_QUEUE_COUNT_MAX       : natural := 1000;
          GC_QUEUE_COUNT_THRESHOLD : natural := 950);

  type t_generic_queue is protected

    procedure add(
      constant instance : in integer;
      constant element  : in t_generic_element);

    procedure add(
      constant element : in t_generic_element);

  end protected;

end package generic_queue_pkg;

package body generic_queue_pkg is

  type t_generic_queue is protected body

    variable vr_num_elements_in_queue : integer_vector(0 to 1) := (others => 0);
    variable vr_queue_count_max       : integer_vector(0 to 1) := (others => GC_QUEUE_COUNT_MAX);

    procedure add(
      constant instance : in integer;
      constant element  : in t_generic_element
    ) is
    begin
      assert vr_num_elements_in_queue(instance) < vr_queue_count_max(instance);
    end procedure;

    procedure add(
      constant element : in t_generic_element
    ) is
    begin
      add(1, element);
    end procedure;

  end protected body;

end package body generic_queue_pkg;

-------------------------------------------------------------------------------

package generic_sb_support_pkg is

  type t_sb_config is record
    allow_lossy               : boolean;
    allow_out_of_order        : boolean;
    overdue_check_time_limit  : time;
    ignore_initial_garbage    : boolean;
  end record;

  type t_sb_config_array is array (integer range <>) of t_sb_config;

  constant C_SB_CONFIG_DEFAULT : t_sb_config := (allow_lossy               => false,
                                                 allow_out_of_order        => false,
                                                 overdue_check_time_limit  => 0 ns,
                                                 ignore_initial_garbage    => false);

end package generic_sb_support_pkg;

package body generic_sb_support_pkg is

end package body generic_sb_support_pkg;


-------------------------------------------------------------------------------

use work.generic_sb_support_pkg.all;

package generic_sb_pkg is

  generic(type t_element;
          constant sb_config_default        : t_sb_config := C_SB_CONFIG_DEFAULT;
          constant GC_QUEUE_COUNT_MAX       : natural     := 1000;
          constant GC_QUEUE_COUNT_THRESHOLD : natural     := 950);

  type t_generic_sb is protected

    procedure add_expected(
      constant instance         : in integer;
      constant expected_element : in t_element;
      constant tag              : in string);

    procedure add_expected(
      constant expected_element : in t_element;
      constant tag              : in string);

  end protected t_generic_sb;

end package generic_sb_pkg;

package body generic_sb_pkg is

  type t_sb_entry is record
    expected_element : t_element;
    source           : string(1 to 5);
    tag              : string(1 to 5);
    entry_time       : time;
  end record;

  package sb_queue_pkg is new work.generic_queue_pkg
    generic map(
      t_generic_element        => t_sb_entry,
      GC_QUEUE_COUNT_MAX       => GC_QUEUE_COUNT_MAX,
      GC_QUEUE_COUNT_THRESHOLD => GC_QUEUE_COUNT_THRESHOLD);

  use sb_queue_pkg.all;

  type t_generic_sb is protected body

   variable vr_sb_queue         : sb_queue_pkg.t_generic_queue;

   procedure add_expected(
      constant instance         : in integer;
      constant expected_element : in t_element;
      constant tag              : in string
    ) is
      variable v_sb_entry : t_sb_entry;
    begin
        vr_sb_queue.add(0, v_sb_entry);
    end procedure add_expected;

    procedure add_expected(
      constant expected_element : in t_element;
      constant tag              : in string
    ) is
    begin
        add_expected(1, expected_element, tag);
    end procedure add_expected;

  end protected body;

end package body generic_sb_pkg;


-------------------------------------------------------------------------------

use work.generic_sb_support_pkg.all;

entity issue858 is
end entity;

architecture tb of issue858 is
    constant C_SLV_SB_CONFIG_DEFAULT : t_sb_config := (allow_lossy               => false,
                                                       allow_out_of_order        => false,
                                                       overdue_check_time_limit  => 0 ns,
                                                       ignore_initial_garbage    => false);

  package slv_sb_pkg is new work.generic_sb_pkg
    generic map(t_element         => bit_vector(7 downto 0),
                sb_config_default => C_SLV_SB_CONFIG_DEFAULT);

  use slv_sb_pkg.all;

  shared variable sb_under_test : slv_sb_pkg.t_generic_sb;
begin
  p_main: process
  begin
    sb_under_test.add_expected(X"01", "tag added");
    wait;
  end process p_main;
end architecture;
