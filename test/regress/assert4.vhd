entity assert4 is
end entity;

architecture test of assert4 is
begin

    process is
        variable n : integer;
    begin
        n := 0;
        wait for 1 ns;
        assert n = 0 report integer'image(100 / n);
        wait;
    end process;

end architecture;
