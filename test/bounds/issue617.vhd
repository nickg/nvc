entity issue617 is
end entity;

architecture rtl of issue617 is

    type sfixed is array (integer range <>) of bit;

    procedure func(y: in bit_vector) is
    begin
    end procedure;

begin
    process
        variable example : sfixed(31 downto -32);
        variable null_range : sfixed(-33 downto -32);
    begin
        func(bit_vector(example));      -- Error
        func(bit_vector(null_range));   -- OK
        wait;
    end process;
end rtl;
