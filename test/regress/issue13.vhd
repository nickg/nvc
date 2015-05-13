entity issue13 is
end entity;

architecture test of issue13 is
    constant c0: bit_vector(7 downto 0) := "10101010";
    type t_array is array (1 downto 0) of bit_vector(7 downto 0);
    constant c1 : t_array := (
        1 => c0, --error
        0 => "10101010");
begin

    process is
        variable cmp : bit_vector(7 downto 0) := "10101010";
    begin
        assert c1(1) = c0;
        assert c1(0) = cmp;
        wait;
    end process;

end architecture;
