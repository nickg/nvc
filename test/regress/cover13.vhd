library ieee;
use ieee.std_logic_1164.all;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


entity leaf_entity is
    port (
        a              : in    std_logic;
        b              : out   std_logic
    );
end leaf_entity;

architecture test of leaf_entity is

begin

    inv : process (a)
    begin
        b <= not a;
    end process;

    process
    begin
        wait for 1 ns;
        report "HALOO";
        wait for 1 ns;
        report "NEXT HALOO";
        wait for 1 ns;
        wait;
    end process;

end architecture;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity not_interesting_entity is
    port (
        a              : in    std_logic;
        b              : out   std_logic
    );
end not_interesting_entity;

architecture test of not_interesting_entity is

begin

    b <= not a;

end architecture;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

entity level_2 is
end entity;

architecture test of level_2 is

begin

    process
    begin
        wait for 1 ns;
        report "Message from level_2";
        wait for 1 ns;
        wait;
    end process;

end architecture;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

entity level_1 is
end entity;

architecture test of level_1 is

begin

    level_2_inst : entity work.level_2;

    not_interesting_entity_inst : entity work.not_interesting_entity
    port map (
        a    => '1',
        b    => open
    );

    process
    begin
        wait for 1 ns;
        report "Message from level_1";
        wait for 1 ns;
        wait;
    end process;

end architecture;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


library ieee;
use ieee.std_logic_1164.all;

entity cover13 is
end cover13;

architecture test of cover13 is

    signal a    : std_logic;
    signal b    : std_logic;

begin

    leaf_entity_inst : entity work.leaf_entity
    port map (
        a    => a,
        b    => b
    );

    not_interesting_entity_inst : entity work.not_interesting_entity
    port map (
        a    => a,
        b    => open
    );

    level_1_inst : entity work.level_1;

    process
    begin
        wait for 1 ns;
        a <= '1';
        wait for 1 ns;
        a <= '0';
        wait for 1 ns;

        wait;
    end process;

end architecture;

