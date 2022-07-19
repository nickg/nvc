package test_pkg is
  type t_segment_type is (
    TYPE_0,
    TYPE_1,
    TYPE_2,
    TYPE_3,
    TYPE_4,
    TYPE_5
    );

  type unsigned is array (natural range <>) of bit;

  type t_data_segment is record
    data_word         : bit_vector(15 downto 0);  -- 0: 16
    word_idx          : unsigned(11 downto 0);    -- 16: 12
    segment_type      : t_segment_type;           -- 28: 1+3
    word_length       : natural range 1 to 2;     -- 32: 4
    crc_check         : boolean;                  -- 36: 1+3
    table             : natural range 1 to 2;     -- 40: 4
  end record;

  type t_data_segment_template is array(t_segment_type) of t_data_segment;
  constant C_SEGMENT_RECORDS : t_data_segment_template := (
    TYPE_0                    => (
      data_word               => (others => '0'),
      word_idx                => (others => '0'),
      segment_type            => TYPE_0,
      word_length             => 1,
      crc_check               => false,
      table                   => 16#a0#),
    TYPE_1                    => (
      data_word               => (others => '0'),
      word_idx                => (others => '0'),
      segment_type            => TYPE_1,
      word_length             => 2,
      crc_check               => false,
      table                   => 1),
    TYPE_2                    => (
      data_word               => (others => '0'),
      word_idx                => (others => '0'),
      segment_type            => TYPE_2,
      word_length             => 1,
      crc_check               => false,
      table                   => 16#a1#),
    TYPE_3                    => (
      data_word               => (others => '0'),
      word_idx                => (others => '0'),
      segment_type            => TYPE_3,
      word_length             => 2,
      crc_check               => true,
      table                   => 16#a2#),
    TYPE_4                    => (
      data_word               => (others => '0'),
      word_idx                => (others => '0'),
      segment_type            => TYPE_4,
      word_length             => 2,
      crc_check               => true,
      table                   => 16#a3#),
    TYPE_5                    => (
      data_word               => (others => '0'),
      word_idx                => (others => '0'),
      segment_type            => TYPE_5,
      word_length             => 2,
      crc_check               => true,
      table                   => 16#a4#)
  );

  constant C_DATA_INVALID_VERSION : bit_vector(7 downto 0)  := x"FF";
  constant C_DATA_VERSION         : bit_vector(C_SEGMENT_RECORDS(TYPE_2).word_length*8-1 downto 0) := C_DATA_INVALID_VERSION;
end package;
