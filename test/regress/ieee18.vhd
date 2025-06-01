-- Division cases
--

library ieee;
use ieee.numeric_std.all;

entity ieee18 is
end entity;

architecture test of ieee18 is
begin

    big: process is
        variable ua, ub, uc : unsigned(199 downto 0);
    begin
        ua := to_unsigned(5, 200);
        ub := to_unsigned(2, 200);
        uc := ua / ub;
        assert uc = ub report to_hstring(uc);
        uc := ua(3 downto 0) rem ub;
        assert uc = 1 report to_hstring(uc);

        wait;
    end process;

    small: process is
        variable ua, ub, uc : unsigned(19 downto 0);
    begin
        wait for 1 ns;

        ua := to_unsigned(5, 20);
        ub := to_unsigned(2, 20);
        uc := ua / ub;
        assert uc = ub report to_hstring(uc);
        uc := ua rem ub;
        assert uc = 1 report to_hstring(uc);

        wait for 1 ns;

        uc := ua / 0;
        assert uc = 0 report to_hstring(uc);

        wait;
    end process;

end architecture;
