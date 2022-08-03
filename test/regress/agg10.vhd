entity agg10 is
end entity;

architecture test of agg10 is
    signal s : bit_vector(15 downto 0);
    signal t : bit_vector(11 downto 0);
begin

    s <= (15 downto 12 => '0',
          11 downto 0 => t);

    process is
    begin
        assert s = X"0000";
        t <= X"fff";
        wait for 1 ns;
        assert s = X"0fff";
        t(0) <= '0';
        wait for 1 ns;
        assert s = X"0ffe";
        wait;
    end process;

end architecture;
