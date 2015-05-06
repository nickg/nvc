package wait_until_pkg is
  procedure wait_until(signal sig : in boolean; val : boolean);
  procedure wait_until(signal sig : in bit_vector; val : bit_vector);
end package;

package body wait_until_pkg is
  procedure wait_until(signal sig : in boolean; val : boolean) is
  begin
    wait until sig = val; -- This does not work
  end procedure;

  function fun(x : bit_vector) return bit_vector is
  begin
      return x;
  end function;

  procedure wait_until(signal sig : in bit_vector; val : bit_vector) is
  begin
    wait until sig = fun(val);
  end procedure;
end package body;

-------------------------------------------------------------------------------

entity issue163 is
end entity;

use work.wait_until_pkg.all;

architecture test of issue163 is
    signal s : boolean;
    signal v : bit_vector(7 downto 0);
begin

    s <= true after 1 ns, false after 2 ns;

    process is
    begin
        wait_until(s, true);
        assert now = 1 ns;
        wait_until(s, false);
        assert now = 2 ns;
        wait;
    end process;

    v <= X"10" after 1 ns, X"bc" after 2 ns;

    process is
    begin
        wait_until(v, X"10");
        assert now = 1 ns;
        wait_until(v, X"bc");
        assert now = 2 ns;
        wait;
    end process;

end architecture;
