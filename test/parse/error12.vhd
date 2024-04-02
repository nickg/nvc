package generic_queue_pkg is

  generic(type t_generic_element;
          GC_QUEUE_COUNT_MAX       : natural := 1000;
          GC_QUEUE_COUNT_THRESHOLD : natural := 950);

end package generic_queue_pkg;

-------------------------------------------------------------------------------

package generic_sb_pkg is

  generic(type t_element;
          function element_match(received_element : t_element;
                                 expected_element : t_element) return boolean;
          function to_string_element(element : t_element) return string;
          constant GC_QUEUE_COUNT_MAX       : natural     := 1000;
          constant GC_QUEUE_COUNT_THRESHOLD : natural     := 950);

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
      scope                    => "SB_queue",
      GC_QUEUE_COUNT_MAX       => GC_QUEUE_COUNT_MAX,
      GC_QUEUE_COUNT_THRESHOLD => GC_QUEUE_COUNT_THRESHOLD);

  use sb_queue_pkg.all;

end package body generic_sb_pkg;
