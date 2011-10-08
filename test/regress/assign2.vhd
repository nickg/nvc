entity assign2 is
end entity;

architecture test of assign2 is
begin

    process is
        variable x : bit_vector(7 downto 0) := (1 => '1', others => '0');
    begin
        assert x(0) = '0';
        assert x(1) = '1';
        assert x(4) = x(5);
        x(2) := '1';
        assert x(2) = '1';
        wait;
    end process;
    
end architecture;
