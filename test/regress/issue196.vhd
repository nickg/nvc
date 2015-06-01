package pkg is
  procedure proc(s : string);
end package;

package body pkg is
  procedure proc(s : string) is
  begin
  end procedure;
end package body;

-------------------------------------------------------------------------------

use work.pkg.all;
entity issue196 is
end entity;

architecture tb of issue196 is
  type rec_t is record
    field : natural;
  end record;

  procedure proc is
  begin
    proc("" & LF);
    wait for 0 ns;
    proc("------------------------------------------------------------" & LF);
  end;

  procedure proc2 (
    constant var : in rec_t := (others => 0)) is
  begin
    wait for 0 ns;
    report integer'image(var.field);
    assert var = (others => 0);
    proc("--------------------------------abcd" & LF);  -- abcd corrupts var

    -- var.field was overwritten by "abcd"
    report integer'image(var.field) & " = " & integer'image((((character'pos('d')*256 + character'pos('c'))*256 + character'pos('b'))*256 + character'pos('a')));
    assert var = (others => 0); -- Should still be 0
  end procedure;
begin
  main : process
  begin
    proc;
    proc2;
    wait;
  end process;
end architecture;
