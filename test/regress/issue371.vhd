library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue371 is
end issue371;

architecture behav of issue371 is
	signal clock : std_logic;
	signal chip_select_sig : std_logic;
	signal address_sig : std_logic_vector(2 downto 0);
	signal write_sig : std_logic;
	signal host_data_bus_sig : std_logic_vector(7 downto 0);

	procedure wait_for_ticks (num_clock_cycles : in integer) is
	begin
		for i in 1 to num_clock_cycles loop
			wait until rising_edge(clock);
		end loop;
	end procedure wait_for_ticks;

	procedure host_write (addr: in std_logic_vector;
		byte : in std_logic_vector;
		signal clock : in std_logic;
		signal chip_select : out std_logic;
		signal address : out std_logic_vector;
		signal write : out std_logic;
		signal host_data_bus : out std_logic_vector;
		invert_cs_etc : in std_logic
	) is
	begin
		wait until rising_edge(clock);
		write <= '1' xor invert_cs_etc;
		chip_select <= '1' xor invert_cs_etc;
		address <= addr;
		host_data_bus <= byte;
		wait_for_ticks(1);

		write <= '0' xor invert_cs_etc;
		chip_select <= '0' xor invert_cs_etc;
		for i in address'LOW to address'HIGH loop
			address(i) <= '0';
		end loop;
		for i in host_data_bus'LOW to host_data_bus'HIGH loop
			host_data_bus(i) <= '0';
		end loop;
		wait until rising_edge(clock);
	end procedure host_write;

begin

	process
	begin
		for i in 0 to 10 loop
			clock <= '0';
			wait for 1 us;
			clock <= '1';
			wait for 1 us;
		end loop;
		wait;
	end process;

	process
	begin
		host_write("001", X"aa", clock, chip_select_sig, address_sig, write_sig, host_data_bus_sig, '0');
		for i in address_sig'LOW to address_sig'HIGH loop
			assert address_sig(i) = '0';
		end loop;
		for i in host_data_bus_sig'LOW to host_data_bus_sig'HIGH loop
			assert host_data_bus_sig(i) = '0';
		end loop;
		wait;
	end process;
end behav;
