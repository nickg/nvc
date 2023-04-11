entity event1 is
end entity;

architecture test of event1 is
    signal s : bit;
    signal t : bit_vector(1 to 2);
begin

    process is
    begin
        assert not s'event;
        assert not t'event;
        s <= '1';
        t <= "11";
        wait for 1 ns;
        wait;
    end process;

end architecture;
