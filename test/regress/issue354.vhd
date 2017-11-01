entity issue354 is
end issue354;

architecture behav of issue354 is
signal byte : bit_vector(7 downto 0);
signal byte_too : bit_vector(7 downto 0);

begin

-- nvc doesn't like the byte_too(1) in the next line
byte(1 downto 0) <= (1 => byte_too(1), 0 => '0') when true else (others => '0');
process
begin
	byte_too(0) <= '0';
	byte_too(1) <= '1';
	wait for 100ns;
	assert byte(0) = '0';
	assert byte(1) = '1';

	assert false report "end of test" severity note;
	wait;
end process;
end behav;
