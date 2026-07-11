library ieee;
use ieee.std_logic_1164.all;

entity dummy is
    port(
        input  : in std_logic;
        output : out std_logic
    );
end entity;

architecture rtl of dummy is
begin
    output <= input;
end architecture;

------------------------------------------------------------------------
-- Top-level example
------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity scope_demo is
end entity;

architecture rtl of scope_demo is

    signal u : std_logic_vector(3 downto 0) := "0000";

begin

    -- this is the problematic part:
    gen : for i in 0 to 3 generate
    begin
        u : entity work.dummy
            port map(
                input  => u(i),
                output => open
            );
    end generate;

    -- this is handled correctly:
    u : entity work.dummy
        port map(
            input  => u(0),
            output => open
        );

    -- extended checks
    ex : block
        constant c0 : std_logic := scope_demo(0);
        constant c1 : std_logic := rtl(0);
        constant c2 : std_logic := ieee(0);
        constant c3 : std_logic := ieee.std_logic_1164(0);
        constant c4 : std_logic_vector(1 downto 0) := scope_demo(1 downto 0);
        constant c5 : std_logic_vector(1 downto 0) := rtl(1 downto 0);
        constant c7 : std_logic_vector(1 downto 0) := ieee(1 downto 0);
        constant c6 : std_logic_vector(1 downto 0) := ieee.std_logic_1164(1 downto 0);
    begin
    end block;

end architecture;
