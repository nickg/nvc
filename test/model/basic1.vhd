entity basic1 is
end entity;

architecture test of basic1 is
    signal x : integer := 42;
    signal y : bit := '1';
begin

    p1: process is
    begin
        x <= x + 1;
        wait;
    end process;

end architecture;
