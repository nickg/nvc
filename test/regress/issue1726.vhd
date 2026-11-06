library ieee;
use ieee.numeric_std.all;

entity issue1726_sub is
    generic ( width : natural );
    port ( x : in unsigned(width - 1 downto 0) );
end entity;

architecture test of issue1726_sub is
begin
    process is
    begin
        wait for 1 ns;
        assert x = to_unsigned(16, x'length);
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.numeric_std.all;

entity issue1726 is
    generic ( frac_bits : natural := 2 );
end entity;

architecture test of issue1726 is
    constant value : signed(2 * frac_bits + 1 downto 0) :=
        to_signed(2 ** (2 * frac_bits), 2 * frac_bits + 2);
begin
    sub: entity work.issue1726_sub
        generic map ( width => 2 * frac_bits + 2 )
        port map ( x => unsigned(value) );
end architecture;
