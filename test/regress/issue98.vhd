entity issue98 is
end entity;

architecture foo of issue98 is
begin
    p1: process is
    begin
        -- 'POS returns universal integer
        --  'VAL parameter is any integer type (including universal integer)

        assert False
            report "Time'HIGH = " & Time'IMAGE(Time'VAL(Time'POS(Time'HIGH)))
            severity NOTE;
        assert False
            report "should produce 9223372036854775807"
            severity NOTE;

        assert Time'VAL(Time'POS(Time'HIGH)) = 9223372036854775807 fs;

        wait;
    end process;

end architecture;
