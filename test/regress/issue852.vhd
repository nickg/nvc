entity issue852 is
end entity;

architecture test of issue852 is

    type data_array_t is array (natural range <>) of bit_vector(31 downto 0);

    type mock_completer_t is record
        -- Configuration elements
        prefix : string; -- Optional prefix used in report messages.
        -- Internal elements
        memory : data_array_t;
        -- Statistics elements
        read_count  : natural; -- Number of read transfers.
        write_count : natural; -- Number of write transfers.
    end record;

    function init (memory_size: natural; prefix: string := "apb: mock completer: ") return mock_completer_t is
        variable mc : mock_completer_t(prefix(1 to prefix'length), memory(0 to memory_size - 1));
    begin
        mc.prefix := prefix;
        return mc;
    end function;

    signal mc : mock_completer_t := init(memory_size => 8);
begin

    mc.memory(4) <= X"deadbeef" after 2 ns;
    mc.memory(7) <= X"cafebabe" after 5 ns;

end architecture;
