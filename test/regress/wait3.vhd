entity wait3 is
end entity;

architecture test of wait3 is
    signal x, y : bit;
begin

    proc_a: process is
    begin
        wait for 1 ns;
        x <= '1';
        wait for 1 ns;
        assert y = '1';
        wait;
    end process;

    proc_b: process is
    begin
        wait on x;
        assert x = '1';
        assert now = 1 ns;
        y <= '1';
        wait;
    end process;
        
    
end architecture;
