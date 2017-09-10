package pack is
    signal reset : bit := '1';
end package;

-------------------------------------------------------------------------------

entity buf is
    port (
        i : in bit;
        o : out bit );
end entity;

architecture behav of buf is
begin
    o <= i after 1 ns;
end architecture;

entity signal14 is
end entity;

use work.pack.all;

architecture test of signal14 is
    signal reset_o : bit;
begin

    u1: entity work.buf
        port map (
            i => reset,
            o => reset_o );

    process is
    begin
        assert reset_o = '0';
        wait for 2 ns;
        assert reset_o = '1';
        wait;
    end process;

end architecture;
