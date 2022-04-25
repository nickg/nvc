package my_logic is
    type std_logic is ('0', '1');
    type std_logic_vector is array (natural range <>) of std_logic;
    type unsigned is array (natural range <>) of std_logic;
    type signed is array (natural range <>) of std_logic;

    function to_integer(x : unsigned) return integer;
end package;

use work.my_logic.all;

entity test is
  port (
    clk_in   : in std_logic;
    reset_in : in std_logic
    );
end entity;

architecture default1 of test is

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
