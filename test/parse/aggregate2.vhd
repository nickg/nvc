package tick is
  type t_data_segment is record end record;
  constant C_SEGMENT_RECORDS : t_data_segment := (
    data'last => (others => '0')  -- Error
  );
end package;

package brack is
  type t_data_segment is record end record;
  constant C_SEGMENT_RECORDS : t_data_segment := (
    data[word => (others => '0')  -- Error
  );
end package;
