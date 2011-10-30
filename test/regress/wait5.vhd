entity wait5 is
end entity;

architecture test of wait5 is
    signal x, y : integer := 0;
begin

    a: process (x) is
    begin
        y <= y + 1;
    end process;

    b: process is
    begin
        wait for 1 ns;
        assert y = 1;
        x <= 1;
        wait for 1 ns;
        x <= 0;
        wait for 1 ns;
        assert y = 3;
        x <= 0;
        wait for 1 ns;
        assert y = 3;        
        wait;
    end process;    
    
end architecture;

    
