library ieee;
use ieee.std_logic_1164.all;

entity sub is
end entity;

architecture test of sub is

    signal a        : std_logic;
    signal b        : std_logic_vector(3 downto 0);
    signal c        : integer;

begin

end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity siginit1 is
end entity;

architecture test of siginit1 is

    signal should_not_be_initialized     : std_logic;

begin

    sub_i : entity work.sub;

    process is

    begin
        wait for 1 ns;

        assert should_not_be_initialized = 'U';

        assert <<signal sub_i.a : std_logic >> = '0';
        assert <<signal sub_i.b : std_logic_vector(3 downto 0) >> = "0000";
        assert <<signal sub_i.c : integer >> = integer'low;

        wait;
    end process;

end architecture;
