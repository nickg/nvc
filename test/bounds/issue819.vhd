package my_logic is
    type std_ulogic is ('0', '1');
    type std_ulogic_vector is array (natural range <>) of std_ulogic;
    type unsigned is array (natural range <>) of std_ulogic;
    type signed is array (natural range <>) of std_ulogic;
end package;

-------------------------------------------------------------------------------

use work.my_logic.all;

package uCPUtypes is
  alias logic is std_ulogic;
  alias logic_vec is std_ulogic_vector;
end package uCPUtypes;

-------------------------------------------------------------------------------

use work.uCPUtypes.all;

entity uCPU is
end entity uCPU;

architecture RTL of uCPU is

    signal rom_data : logic_vec(11 downto 0);

begin

--------------- combination logic ----------------

    process (rom_data) is
    begin
        case rom_data(11 downto 10) is
            when "00" => null;
            when others => null;
        end case;
    end process;
end architecture RTL;
