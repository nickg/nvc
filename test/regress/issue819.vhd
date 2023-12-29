package my_logic is
    type std_ulogic is ('U', '0', '1', 'X');
    type std_ulogic_vector is array (natural range <>) of std_ulogic;
    type unsigned is array (natural range <>) of std_ulogic;
    type signed is array (natural range <>) of std_ulogic;
end package;

library ieee;
use work.my_logic.all;

package uCPUtypes is
  alias logic is std_ulogic;
  alias logic_vec is std_ulogic_vector;
  subtype unsigned_byte is unsigned(7 downto 0);
  subtype code_word is unsigned(11 downto 0);
end package uCPUtypes;

-------------------------------------------------------------------------------

library ieee;
use work.uCPUtypes.all;
use work.my_logic.all;

entity issue819 is
end entity;

architecture test of issue819 is
    signal rom_data : code_word;
    alias op      : unsigned(2 downto 0) is rom_data(11 downto 9);  -- opcode

    signal alu_arg, alu_res : unsigned_byte;
    signal alu_c : logic;

begin

    with op(1 downto 0) select
        (alu_c, alu_res) <= '0' & alu_arg when "00",
        ('X', x"XX")  when others;

    check: process is
    begin
        wait for 1 ns;
        assert alu_c = 'X';
        assert alu_res = X"XX";
        wait;
    end process;

end architecture;
