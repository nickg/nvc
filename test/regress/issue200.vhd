package pkg is
    procedure proc(length : integer);
end package;

package body pkg is
    procedure proc(length : integer) is
      -- Runtime error
      variable bv : bit_vector(length-1 downto 0) := (others => '0');
    begin
      report integer'image(bv'length);
    end procedure;
end package body;

use work.pkg.all;

entity issue200 is
end entity;

architecture a of issue200 is
begin
  main : process
    -- Static error
    variable bv : bit_vector(-1 downto 0) := (others => '0');
  begin
    report integer'image(bv'length);
    proc(0);
    wait;
  end process;
end architecture;
