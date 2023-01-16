entity issue590 is
end entity;

architecture test of issue590 is
begin

    p1: process is
    begin
        report time'image(time'low);
        assert time'image(time'low) = "-9223372036854775808 fs";
        wait;
    end process;

end architecture;
