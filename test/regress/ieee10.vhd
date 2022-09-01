-- file numeric_std_tb1.vhd is a simulation testbench for
-- IEEE 1076.3 numeric_std package.
-- This is the first in a series of testbenches.
--
library IEEE;

use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ieee10 is
  generic (
    quiet : boolean := false);  -- make the simulation quiet
end entity ieee10;

architecture t1 of ieee10 is

begin
  process
    -- required A15_38
    variable res4,sgn4: signed(3 downto 0);
    variable sgn6: signed(5 downto 0);
    variable res8: signed(7 downto 0);
    variable sgn10,res10:signed(9 downto 0);
    variable ures4,uns4: unsigned(1 to 4);
    variable uns6: unsigned(2 to 7);
    variable uns8: unsigned(0 to 7);
    variable uns10,ures10:unsigned(1 to 10);


  begin

    -- Id: A.21, A.23, and A.24
    for i in 0 to 1023 loop
      uns10:=to_unsigned(i,10);
      for j in 1 to 15 loop
        uns4:=to_unsigned(j,4);
        ures10:=uns10/uns4;
        assert to_integer(ures10)=i/j report "A.21 fails"
          severity FAILURE;
        ures10:=uns10/j;
        assert to_integer(ures10)=i/j report "A.23 fails"
          severity FAILURE;
        ures10:=i/("000000"&uns4);
        assert to_integer(ures10)=i/j report "A.24 fails"
          severity FAILURE;
      end loop;
    end loop;

    if now < 2 ns then
        wait for 1 ns;
    else
        wait;
    end if;
  end process;

end architecture t1;
