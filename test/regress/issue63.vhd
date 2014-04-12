package pack0 is
    constant v : bit_vector := "10";
end package;

entity issue63 is
end entity;

use work.all;

architecture test of issue63 is
begin
    process is
    begin
        assert pack0.v = "10";
        wait;
    end process;
end architecture;
