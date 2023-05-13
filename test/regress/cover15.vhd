entity cover15 is
end cover15;

architecture test of cover15 is

begin

    process
        function crop_and_saturate_time(p : in time) return integer is
        begin
            return 0 when (p < 10 ns) else
                   10 when (p < 20 ns) else
                   20 when (p < 30 ns) else
                   30 when (p < 40 ns) else
                   40;
        end function;
    begin
        wait for 1 ns;
        report integer'image(crop_and_saturate_time(now));
        wait for 10 ns;
        report integer'image(crop_and_saturate_time(now));
        wait for 10 ns;
        report integer'image(crop_and_saturate_time(now));

        wait for 1 ns;
        wait;
    end process;

end architecture;

