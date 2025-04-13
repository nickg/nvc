package str is
  signal s : integer;
end package str;

-------------------------------------------------------------------------------

use work.str.all;

package const is
  type unsigned is array (natural range <>) of bit;
  constant C_ADDRESS : unsigned(4 downto 0) := "00001";
end package const;

-------------------------------------------------------------------------------

use work.const.all;

entity test is
end entity;

architecture beh of test is
  signal   address        : bit_vector(4 downto 0);
begin

  p_proc : process
  begin
    assert C_ADDRESS = "00001" report "Address 1 wrong " & to_string(C_ADDRESS) severity failure;
    address        <= bit_vector(C_ADDRESS);
    assert C_ADDRESS = "00001" report "Address 2 wrong " & to_string(C_ADDRESS) severity failure;
    wait;
  end process;
end architecture;
