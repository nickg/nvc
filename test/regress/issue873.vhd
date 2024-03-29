entity ent is
generic (
    DWIDTH : natural := 4;
    OFFSET : natural := 10
);
port (
    enable : in bit;
    in_data : in bit_vector(DWIDTH+OFFSET-1 downto OFFSET);
    out_data : out bit_vector(DWIDTH+OFFSET-1 downto OFFSET)
);
end entity;

architecture arch of ent is
    function apply_enable(data : bit_vector; en : bit) return bit_vector is
        variable mask : bit_vector(data'range);
        variable data_masked : bit_vector(data'range);
    begin
        mask := (data'length-1 downto 0 => en);
        data_masked := data and mask;
        return data_masked;
    end function;
begin
    out_data <= apply_enable(in_data, enable);
end architecture arch;

-------------------------------------------------------------------------------

entity issue873 is
end entity;

architecture test of issue873 is
    signal enable : bit;
    signal in_data : bit_vector(3 downto 0);
    signal out_data : bit_vector(3 downto 0);
begin

    uut: entity work.ent
        port map (enable, in_data, out_data);

    check: process is
    begin
        wait for 0 ns;
        assert out_data = "0000";
        in_data <= "1010";
        enable <= '1';
        wait for 1 ns;
        assert out_data = "1010";
        enable <= '0';
        wait for 1 ns;
        assert out_data = "0000";
        wait;
    end process;

end architecture;
