entity signal1 is
end entity;

architecture test of signal1 is
    signal x : integer := 5;
begin

    p1: process is
    begin
        assert x = 5;
        x <= 6;
        wait;
    end process;

end architecture;
