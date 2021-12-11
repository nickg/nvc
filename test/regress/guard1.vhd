entity guard1 is
end entity;

architecture test of guard1 is
    signal value  : natural := 0;
    signal output : natural;
begin

    b1: block (value < 10) is
    begin
        output <= guarded value * 2;
    end block;

    check: process is
    begin
        value <= 3;
        wait for 1 ns;
        assert output = 6;
        value <= 4;
        wait for 1 ns;
        assert output = 8;
        value <= 10;
        wait for 1 ns;
        assert output = 8;
        wait;
    end process;

end architecture;
