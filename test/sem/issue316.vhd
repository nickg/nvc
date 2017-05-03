library ieee;
use ieee.std_logic_1164.all;

entity test is
  port (
    clk_in   : in std_logic;
    reset_in : in std_logic
    );
end entity;

architecture default of test is

  type rec_t is record
    field : std_logic;
  end record;

  procedure a_procedure (
    signal reg_in : in rec_t
    ) is
  begin
    null;
  end procedure;


begin

  comb_proc : process
  begin
    a_procedure(
      reg_in => (
        field => '0'
        ));
  end process;

end architecture;
