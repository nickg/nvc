entity issue1270 is
end entity;

architecture rtl of issue1270 is

    type sample_record is record
        element_1 : bit;
        element_2 : integer;
        element_3 : real;
    end record;

    type sample_record_array is array (4 downto 0) of sample_record;

    constant sample_record_default : sample_record := ('0', 0, 0.0);

    signal sample_record_array_signal : sample_record_array := (others => ('1', 42, 0.5));
begin

    p1: process is
    begin
        sample_record_array_signal <= sample_record_array_signal(3 downto 0) & sample_record_default;
        wait for 1 ns;

        -- TODO: check index range here
        assert sample_record_array_signal = (0 to 3 => ('1', 42, 0.5), 4 => ('0',0,0.0));

        assert sample_record_array_signal'last_value = (0 to 4 => ('1', 42, 0.5));
        assert sample_record_array_signal(3 downto 0)'last_event = 1 ns;
        assert sample_record_array_signal(3 downto 0)'last_active = 1 ns;

        wait;
    end process;

end architecture;
