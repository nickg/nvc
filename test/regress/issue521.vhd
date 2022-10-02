entity issue521 is
end entity;

architecture test of issue521 is
    signal i : natural;
    signal j : natural;
begin

    p1: process is
    begin
        i <= i + 1;
        wait for 0 ns;
    end process;

    p2: process (i) is
    begin
        j <= i;
    end process;

end architecture;
