package pack is
    type channel_t is (A, B, C, D);
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    generic ( channel : in channel_t );
    port ( o : out natural );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        case channel is
            when A => o <= 1;
            when B => o <= 24;
            when D => o <= 5;
            when others => o <= 99;
        end case;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity case2 is
end entity;

architecture test of case2 is
    type rec_t is record
        channel : natural;
    end record;

    function get_vals return rec_t is
    begin
        return (channel => 3);
    end function;

    signal s : natural;
begin

    u: entity work.sub generic map ( C ) port map ( s );

end architecture;
