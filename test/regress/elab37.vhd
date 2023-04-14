package pack is

    type params_t is record
        width : natural;
    end record;

end package;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.pack.all;

entity sub is
    generic (
        depth : natural;
        params : params_t );
    port ( clk : inout std_logic := 'Z' );
end entity;

architecture test of sub is
begin

    g1: if depth > 0 generate
        u: entity work.sub
            generic map ( depth - 1, params )
            port map ( clk );
    end generate;

    g2: if depth = 0 generate
        signal s : bit_vector(1 to params.width);
    begin
        p1: process is
        begin
            assert clk = '0';
            assert s'length = 5;
            s <= (others => '1');
            wait for 1 ns;
            assert clk = '1';
            wait;
        end process;
    end generate;

end architecture;

-------------------------------------------------------------------------------

entity elab37 is
end entity;

use work.pack.all;

library ieee;
use ieee.std_logic_1164.all;

architecture test of elab37 is
    constant params : params_t := ( width => 5 );
    signal vec : bit_vector(1 to 5);
    signal clk : std_logic := '0';
begin

    u: entity work.sub
        generic map ( 2, params )
        port map ( clk );

    check: process is
    begin
        clk <= '1';
        wait for 1 ns;
       -- assert vec = "11111";
        wait;
    end process;

end architecture;
