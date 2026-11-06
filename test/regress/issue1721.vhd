entity issue1721 is
end entity;

architecture test of issue1721 is
    signal source : bit := '0';
    signal result : bit;
begin
    process
    begin
        source <= '1';
        wait;
    end process;

    process (all) is
        impure function read_source return bit is
        begin
            return source;
        end function;
    begin
        result <= read_source;
    end process;

    process
    begin
        wait for 1 ns;
        assert result = '1';
        wait;
    end process;
end architecture;
