package function_pkg is
    function get_width return natural;
end function_pkg;

package body function_pkg is
    function get_width return natural is
    begin
        return 5;
    end get_width;
end function_pkg;

use work.function_pkg.all;

package foo is
    type t_record is record
        descriptor: bit_vector(get_width-1 downto 0);
    end record;

    type t_array is array (natural range<>) of t_record;
end package foo;

use work.foo.all;

entity issue962 is
end entity;

architecture rtl of issue962 is
    signal i_channel_states : t_array(0 to 1);
begin
    stim: process is
    begin
	wait for 1 ns;
	i_channel_states(1) <= (descriptor => "10110");
	wait for 1 ns;
	i_channel_states(0).descriptor(0) <= '1';
	wait for 1 ns;
	wait;
    end process;

end architecture;
