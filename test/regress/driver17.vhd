entity driver17 is
end entity;

architecture test of driver17 is
    signal s : bit_vector(1 to 8);
begin

    p1: process is
    begin
        s <= X"00";
        for i in 1 to 8 loop
            s(i) <= '1';
        end loop;
        wait for 1 ns;
        assert s = X"ff";
        s(4) <= '0' after 0 ns, '1' after 2 ns;
        s(1 to 3) <= "111";
        wait for 1 ns;
        assert s = "11101111";
        s(5 to 8) <= X"f";
        wait for 1 ns;
        assert s = X"ff";
        wait;
    end process;

end architecture;
