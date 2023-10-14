package issue768 is

    type t_slv_arr is array (natural range <>) of bit_vector;  --! Array of bit_vector is the primary data type

    type t_code_word is record
        word : bit_vector;            --! data word
        code : t_slv_arr;                   --! code word array
    end record t_code_word;

    type t_codec is array(natural range <>) of t_code_word;  --! Codec (array of data & code words)

    constant C_HDLC_CODEC : t_codec(0 to 0)(word(7 downto 0), code(0 to 0)(7 downto 0)) := (0 => (word => x"7e", code => (0 => x"7d")));

end package;
