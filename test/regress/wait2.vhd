entity wait2 is
end entity;

architecture test of wait2 is
begin

    proc1: process is
    begin
        wait for 1 ns;
        assert now = 1 ns report "a";
        wait for 2 ns;
        assert now = 3 ns report "b";
        wait for 0 fs;
        assert now = 3 ns report "h";
        wait for 0 fs;
        assert now = 3 ns report "i";
        report "done proc1";
        wait;
    end process;

    proc2: process is
    begin
        wait for 1500 ps;
        assert now = 1500 ps report "c";
        wait for 1 ns;
        assert now = 2500 ps report "d";
        wait for 0 fs;
        assert now = 2500 ps report "d";
        wait for 0 fs;
        assert now = 2500 ps report "e";
        wait for 500 ps;
        assert now = 3 ns report "f";
        wait for 0 fs;
        assert now = 3000 ps report "g";
        report "done proc2";
        wait;
    end process;
    
end architecture;
