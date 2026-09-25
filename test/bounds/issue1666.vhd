library ieee;
use ieee.std_logic_1164.all;

package pkg is
    constant c_bit_or   : bit_vector(1 downto 0) := "00" or   "000";
    constant c_bit_nor  : bit_vector(1 downto 0) := "00" nor  "000";
    constant c_bit_and  : bit_vector(1 downto 0) := "00" and  "000";
    constant c_bit_nand : bit_vector(1 downto 0) := "00" nand "000";
    constant c_bit_xor  : bit_vector(1 downto 0) := "00" xor  "000";
    constant c_bit_xnor : bit_vector(1 downto 0) := "00" xnor "000";

    -- TODO: These should also produce errors while parsing.
    constant c_slv_or   : std_logic_vector(1 downto 0) := "00" or   "000";
    constant c_slv_nor  : std_logic_vector(1 downto 0) := "00" nor  "000";
    constant c_slv_and  : std_logic_vector(1 downto 0) := "00" and  "000";
    constant c_slv_nand : std_logic_vector(1 downto 0) := "00" nand "000";
    constant c_slv_xor  : std_logic_vector(1 downto 0) := "00" xor  "000";
    constant c_slv_xnor : std_logic_vector(1 downto 0) := "00" xnor "000";

    type boolean_vector is array (natural range <>) of boolean;
    constant c_bool_or   : boolean_vector(1 downto 0) := (FALSE, FALSE) or   (FALSE, FALSE, FALSE);
    constant c_bool_nor  : boolean_vector(1 downto 0) := (FALSE, FALSE) nor  (FALSE, FALSE, FALSE);
    constant c_bool_and  : boolean_vector(1 downto 0) := (FALSE, FALSE) and  (FALSE, FALSE, FALSE);
    constant c_bool_nand : boolean_vector(1 downto 0) := (FALSE, FALSE) nand (FALSE, FALSE, FALSE);
    constant c_bool_xor  : boolean_vector(1 downto 0) := (FALSE, FALSE) xor  (FALSE, FALSE, FALSE);
    constant c_bool_xnor : boolean_vector(1 downto 0) := (FALSE, FALSE) xnor (FALSE, FALSE, FALSE);
end package;
