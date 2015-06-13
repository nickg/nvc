use std.textio.all;

entity issue203 is
end entity;

architecture a of issue203 is
begin
  main : process
    procedure proc is
    begin
      write(output, string'("hello") & LF);      -- Crash here
    end procedure;
  begin
    proc;
    wait;
  end process;
end architecture;
