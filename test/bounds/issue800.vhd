entity issue800 is
end entity;

architecture test of issue800 is
    signal x : bit_vector(1 to 3);
begin

    p1: process is
    begin
        assert x = "00";                -- Warning
        assert x /= "101010101";        -- Warning
        assert x = "000";               -- OK
        assert string'("101") = "1111"; -- No warning (folded)
        assert "111" = x(1 to 1);       -- Warning
        wait;
    end process;

end architecture;
