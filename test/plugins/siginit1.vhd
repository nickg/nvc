library ieee;
use ieee.std_logic_1164.all;

entity sub is
end entity;

architecture test of sub is

    type t_enum is (
        ENUM_A,
        ENUM_B,
        ENUM_C,
        ENUM_D
    );

    signal a : std_logic;
    signal b : std_logic_vector(3 downto 0);
    signal c : integer;
    signal e : t_enum;

begin

    process is
    begin
        wait for 0 ns;

        assert a = '0';
        assert b = "0000";
        assert c = integer'left;
        assert e = ENUM_A;

        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity siginit1 is
end entity;

architecture test of siginit1 is

    -- This signal must NOT be initialized
    -- (top entity is not the filtered block)
    signal should_not_be_initialized : std_logic;

begin

    sub_i : entity work.sub;

    process is
    begin
        wait for 0 ns;

        assert should_not_be_initialized = 'U';

        wait;
    end process;

end architecture;
