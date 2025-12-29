entity ieee19 is
end entity;

library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

architecture test of ieee19 is
begin

    -- Large arithmetic cases
    process is
        variable a, b, c : unsigned(239 downto 0);
    begin
        a(119 downto 0) := X"012345678abcdef" & X"012345678abcdef";
        b := (1 => '1', others => '0');
        c(119 downto 0) := a(119 downto 0) + 0;
        assert c(119 downto 0) = a(119 downto 0) report to_hstring(c(119 downto 0));
        c(119 downto 0) := a(119 downto 0) + X"1";
        assert c(119 downto 0) = X"012345678ABCDEF012345678ABCDF0" report to_hstring(c(119 downto 0));
        c(239 downto 0) := a(119 downto 0) * b(119 downto 0);
        assert c(119 downto 0) = X"02468ACF1579BDE02468ACF1579BDE" report to_hstring(c(119 downto 0));

        wait;
    end process;

    -- Logical operations
    process is
        variable x, y : std_logic;
    begin
        x := '1';
        y := 'L';
        wait for 1 ns;
        assert (x and y) = '0';
        assert (x or y) = '1';
        assert (x or 'U') = '1';
        assert (x and 'X') = 'X';
        assert (x xor y) = '1';
        assert (x nand y) = '1';
        assert (x nor y) = '0';
        assert (x nor 'U') = '0';
        assert (x nand 'X') = 'X';
        assert (x xnor y) = '0';
        wait;
    end process;

end architecture;
