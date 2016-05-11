entity jcore3 is
end entity;

architecture test of jcore3 is
    signal x, y : integer;
begin

    a: process (x, y) is
        variable count : integer := 0;
    begin
        report "wakeup";
        count := count + 1;
        assert count <= 2;
    end process;

    b: process is
    begin
        x <= 1;
        wait;
    end process;

    c: process is
    begin
        y <= 1;
        wait;
    end process;

end architecture;
