entity driver16 is
end entity;

architecture test of driver16 is
    signal s : bit_vector(1 to 3);
begin

    p1: process is
        variable i : natural := 2;
    begin
        s <= "101";
        s(i) <= '1';
        wait for 0 ns;
        assert s = "111";
        wait;
    end process;

end architecture;
