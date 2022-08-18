package test_pkg is
  type t_if is record
    clk    : bit;
    txd    : bit_vector(7 downto 0);
    txen   : bit;
  end record;
end package;

use work.test_pkg.all;

entity test1 is
end entity;

architecture beh of test1 is
  signal sig  : t_if;
begin
end architecture;

use work.test_pkg.all;

entity issue552 is
end entity;

architecture beh of issue552 is
begin
  i_test : entity work.test1;

  p_proc : process
    alias sig    is <<signal i_test.sig : t_if >>;
  begin
    wait until sig.txen = '1' for 1.1 us;
    assert sig.txen = '0';
    sig <= force ('1', X"00", '1');
    wait for 0 ns;
    assert sig.txen = '1';
    wait;
  end process;
end architecture;
