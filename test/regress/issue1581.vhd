library IEEE;
use     IEEE.numeric_std.all;

entity issue1581 is
end entity;

architecture a of issue1581 is
	signal ID1 : unsigned(0 downto 1);
	signal ID2 : unsigned(0 downto 1);
	signal ID3 : signed(0 downto 1);
	signal ID4 : signed(0 downto 1);
begin
	ID2 <= ID1 + 1;
	ID4 <= ID3 + 1;
end architecture;
