entity assert5 is
end entity;

architecture test of assert5 is
    signal x : integer;
begin

    postponed assert x < 10;

    process is
    begin
        x <= 5;
        wait for 1 ns;
        x <= 11;
        wait for 0 ns;
        x <= 20;
        wait for 0 ns;
        x <= 1;
        wait;
    end process;

end architecture;
