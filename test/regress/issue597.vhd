library ieee;
use ieee.std_logic_1164.all;

package pack is
    type rec2_t is record
        x : std_logic;
    end record;

    type rec_t is record
        r : rec2_t;
        y : std_logic_vector(1 to 3);
    end record;

end package;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.pack.all;

entity sub is
    port (
        o1 : inout std_logic;
        o2 : out rec_t;
        o3 : out std_logic_vector(1 to 2) := "11";
        o4 : out std_logic_vector(1 to 2) );
end entity;

architecture test of sub is
begin
    o4(1) <= '1';
end architecture;

-------------------------------------------------------------------------------

entity issue597 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

use work.pack.all;

architecture test of issue597 is
    signal s1 : std_logic;
    signal s2 : rec_t;
    signal s3 : std_logic_vector(1 to 2);
    signal s4 : std_logic_vector(1 to 2);
begin

    u: entity work.sub
        port map ( s1, s2, s3, s4 );

    p1: process is
    begin
        s1 <= '1';
        s2 <= ( ( x => '1'), y => "111" );
        s3 <= s3;
        s4 <= s4;
        wait;
    end process;

end architecture;
