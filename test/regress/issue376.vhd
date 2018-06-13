entity decoder is
	port(
		i_bcd     : in bit_vector(3 downto 0); -- bcd input
		o_display : out bit_vector(6 downto 0) -- display output
	);
end entity;

library ieee;
use ieee.numeric_bit.all;

architecture rtl of decoder is
	constant D_ZERO   : bit_vector(6 downto 0) := "1111110";
	constant D_ONE    : bit_vector(6 downto 0) := "0110000";
	constant D_TWO    : bit_vector(6 downto 0) := "1101101";
	constant D_THREE  : bit_vector(6 downto 0) := "1111001";
	constant D_FOUR   : bit_vector(6 downto 0) := "0110011";
	constant D_FIVE   : bit_vector(6 downto 0) := "1011011";
	constant D_SIX    : bit_vector(6 downto 0) := "1011111";
	constant D_SEVEN  : bit_vector(6 downto 0) := "1110000";
	constant D_EIGHT  : bit_vector(6 downto 0) := "1111111";
	constant D_NINE   : bit_vector(6 downto 0) := "1111011";
	constant D_E      : bit_vector(6 downto 0) := "1111001";

	type t_decoder_arr is array (0 to 15) of bit_vector(6 downto 0);
	constant decoder_arr : t_decoder_arr :=	(D_ZERO, D_ONE, D_TWO, D_THREE, D_FOUR, D_FIVE,
                                                 D_SIX, D_SEVEN, D_EIGHT, D_NINE, D_E, D_E, D_E, D_E, D_E, D_E);
begin -- architecture
	with to_integer(unsigned(i_bcd)) select
            o_display <=
                  decoder_arr(to_integer(unsigned(i_bcd))) when 0 to 15,
			D_E when others;
end architecture;

-------------------------------------------------------------------------------

entity issue376 is
end entity;

architecture test of issue376 is
    signal i_bcd     : bit_vector(3 downto 0);
    signal o_display : bit_vector(6 downto 0);
begin
    decoder_1: entity work.decoder
        port map (
            i_bcd     => i_bcd,
            o_display => o_display);

    process is
    begin
        i_bcd <= "0000";
        wait for 1 ns;
        assert o_display = "1111110";
        i_bcd <= "0001";
        wait for 1 ns;
        assert o_display = "0110000";
        wait;
    end process;
end architecture;
