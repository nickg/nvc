entity assign3 is
end entity;

architecture test of assign3 is
begin

    p1: process is
        variable x : bit_vector(7 downto 0);
        variable y : bit_vector(7 downto 0);
    begin
        x := y;
        assert x /= y;
        wait;
    end process;

end architecture;
