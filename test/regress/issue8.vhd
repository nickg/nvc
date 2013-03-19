package pack is
    constant results : bit_vector(1 downto 0):="11";
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity issue8 is
end entity;

architecture test of issue8 is
    signal bv : bit_vector(1 downto 0);
begin
    bv <= results;

    process is
    begin
        wait for 1 ns;
        assert bv = "11";
        wait;
    end process;

end architecture;
