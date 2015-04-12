entity issue115 is
end issue115;

architecture behav of issue115 is

    signal PC_OUT_bus         : BIT_VECTOR (7 DOWNTO 0);
    signal tmp                : BIT_VECTOR (7 DOWNTO 0);

    begin

    process
        procedure pc_read (signal register_data : out bit_vector(7 downto 0)) is
        begin
            register_data <= PC_OUT_bus;
        end procedure pc_read;

    begin
        PC_OUT_BUS <= X"ab";
        wait for 1 ns;
        pc_read(tmp);
        wait for 1 ns;
        assert tmp = X"ab";
        wait;
    end process;
end behav;
