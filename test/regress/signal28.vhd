entity signal28 is
end entity;

architecture test of signal28 is

    procedure set_bit (signal x : out bit) is
    begin
        x <= '1';
    end procedure;

    signal s : bit_vector(1 to 7);
begin

    p1: process is
    begin
        for i in s'range loop
            set_bit(s(i));              -- OK with relaxed rules
        end loop;
        wait for 1 ns;
        assert s = "1111111";
        wait;
    end process;

end architecture;
