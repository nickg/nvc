library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity core is
    port (
        a, b : in signed(7 downto 0);
        y    : out signed(7 downto 0);
        ovr  : out std_logic
    );
end entity;

architecture behav of core is
    type t_result is record
        ovr : std_logic;
        y : signed; -- line 16
    end record;

    function signed_add_with_overflow(a, b : signed) return t_result is
        variable y : a'subtype; -- line 20
        variable ovr : std_logic;
    begin
        y := a + b;
        ovr := not (a(a'left) xor b(b'left)) and (a(a'left) xor y(y'left));
        return (ovr, y);
    end function signed_add_with_overflow;
begin
    process (all) is
    begin
        (ovr, y) <= signed_add_with_overflow(a, b);
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue1453 is
end entity;

architecture tb of issue1453 is
    signal a, b : signed(7 downto 0);
    signal y    : signed(7 downto 0);
    signal ovr  : std_logic;

begin
    dut: entity work.core
        port map (
            a   => a,
            b   => b,
            y   => y,
            ovr => ovr
        );

    stim: process
    begin
        a <= to_signed(1, 8);
        b <= to_signed(2, 8);
        wait for 1 ns;
        assert y = to_signed(3, 8) severity failure;
        assert ovr = '0' severity failure;

        a <= to_signed(100, 8);
        b <= to_signed(100, 8);
        wait for 1 ns;
        assert y = to_signed(-56, 8) severity failure;
        assert ovr = '1' severity failure;

        a <= to_signed(-120, 8);
        b <= to_signed(-120, 8);
        wait for 1 ns;
        assert y = to_signed(16, 8) severity failure;
        assert ovr = '1' severity failure;

        a <= to_signed(127, 8);
        b <= to_signed(-1, 8);
        wait for 1 ns;
        assert y = to_signed(126, 8) severity failure;
        assert ovr = '0' severity failure;

        wait;
    end process;
end architecture;
