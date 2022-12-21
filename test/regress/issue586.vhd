package pack is
    type rec_t is record
        x : bit_vector;
    end record;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( r : in rec_t );
end entity;

architecture test of sub is
begin

end architecture;

-------------------------------------------------------------------------------

entity issue586 is
end entity;

use work.pack.all;

architecture test of issue586 is
    signal s : bit_vector(1 to 3);

    type rec2_t is record
        x : bit_vector;
    end record;

    type rec3_t is record
        r : rec2_t;
    end record;

    signal r : rec3_t(r(x(1 to 5)));
begin

    u1: entity work.sub port map ( r.x => s );

    u2: entity work.sub port map ( r.x => r.r.x );

    stim: process is
    begin
        wait for 1 ns;
        r.r.x(3) <= '1';
        wait for 1 ns;
        wait;
    end process;

end architecture;
