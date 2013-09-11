package test2 is
    constant C0 : bit_vector(1 downto 0) := '0' & '1';
    constant C1 : bit_vector(2 downto 0) := C0 & '1';
end test2;

entity issue10 is
end entity;

use work.test2.all;

architecture test of issue10 is
begin

    process is
    begin
        assert C0 = "01";
        assert C1 = "011";
        wait;
    end process;

end architecture;
