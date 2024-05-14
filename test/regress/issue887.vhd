entity issue887 is
end entity;

architecture test of issue887 is

    function CalcPercentCov( Count : integer ; AtLeast : integer ) return real is
        variable PercentCov : real ;
    begin
        if AtLeast > 0 then
            return real(Count)*100.0/real(AtLeast) ;
        elsif AtLeast = 0 then
            return 100.0 ;
        else
            return real'right ;
        end if ;
    end function CalcPercentCov ;

begin

    check: process is
        variable sum : real := 0.0;
    begin
        -- Run enough times for JIT to kick in
        for i in 1 to 1000 loop
            sum := sum + CalcPercentCov(i, 100);
        end loop;
        report real'image(sum);
        assert sum = 500500.0;
        wait;
    end process;

end architecture;
