entity record_name is
end entity record_name;

architecture rtl of record_name is
    subtype byte is bit_vector(7 downto 0);
    type byte_vector is array (natural range <>) of byte;
    type my_record is record
        my_record : byte_vector(0 to 4);
    end record my_record;
    constant MY_CONST : my_record := (my_record => (others => x"00"));  -- OK
    constant test2 : my_record := (integer => 1);  -- Error
begin
end architecture rtl;
