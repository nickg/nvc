entity alias7 is
end entity;

architecture test of alias7 is
    signal x   : bit_vector(7 downto 0);
    alias top is x(7);
    signal ctr : integer := 0;
begin

    process (top) is
    begin
        if top = '1' then
            ctr <= ctr + 1;
        end if;
    end process;

    process is
    begin
        assert ctr = 0;
        x <= X"3f";
        wait for 1 ns;
        assert ctr = 0;
        x <= X"80";
        wait for 1 ns;
        assert ctr = 1;
        x <= X"00";
        wait for 1 ns;
        assert ctr = 1;
        x <= X"ff";
        wait for 1 ns;
        assert ctr = 2;
        wait;
    end process;

end architecture;
