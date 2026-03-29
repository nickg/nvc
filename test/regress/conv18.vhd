entity conv18 is
end entity;

architecture test of conv18 is
    function count_ones(x : bit_vector) return natural is
        variable sum : natural := 0;
    begin
        for i in x'range loop
            if x(i) = '1' then
                sum := sum + 1;
            end if;
        end loop;
        return sum;
    end function;

    signal s : natural;
begin

    b: block is
        port (p : out bit_vector(7 downto 0));
        port map (count_ones(p) => s);
    begin
        stim: process is
        begin
            p <= X"14" after 1 ns;
            wait for 1 ns;
            p <= X"ff" after 2 ns;
            wait for 2 ns;
            p(5) <= '1';
            wait;
        end process;
    end block;

    check: process is
    begin
        wait for 1 ns;
        assert s'active;
        assert s'last_active = 0 ns;
        assert s = 2;
        wait for 1 ns;
        assert not s'active;
        assert s'last_active = 1 ns;
        wait for 1 ns;
        assert s'active;
        assert s'last_active = 0 ns;
        assert s = 8;
        wait for 0 ns;
        assert s'active;
        assert s'last_active = 0 ns;
        assert s = 8;
        wait;
    end process;

end architecture;
