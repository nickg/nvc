entity mixed3 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed3 is
    component dff is
	port ( q : out std_logic;
	       cp, d : in std_logic );
    end component;

    signal q, cp, d : std_logic := '0';
    signal running : boolean;
begin

    uut: component dff
	port map ( q, cp, d );

    clkgen: cp <= not cp after 5 ns when running;

    check: process is
    begin
	assert q = '1';
	wait for 0 ns;
	assert q = '1';
	wait until falling_edge(cp);
	assert q = '0';
	wait for 1 ps;
	assert q = '0';
	d <= '1';
	wait for 1 ps;
	assert q = '0';
	wait until falling_edge(cp);
	assert q = '1';

	running <= false;
	wait;
    end process;

end architecture;
