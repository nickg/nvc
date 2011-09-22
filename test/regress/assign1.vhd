entity assign1 is
end entity;

architecture test of assign1 is
begin

    process is
        variable x, y : integer;
    begin
        x := 5;
        y := 7;
        assert x = 5;
        assert y = 7;
        wait for 1 ns;
        assert x + y = 12;
        wait;
    end process;

    process is
        variable x : integer := 64;
        variable y : integer := -4;
    begin
        wait for 4 ns;
        assert x = 64 report "x not 64";
        assert y = -4 report "y not -4";
        x := y * 2;
        assert x = -8;
        wait;
    end process;
    
end architecture;
