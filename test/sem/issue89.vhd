-- -*- vhdl-basic-offset: 2 -*-
entity double_alias is
end entity;

architecture a of double_alias is
  procedure proc(arg : integer) is
  begin
  end procedure;

  procedure proc(arg : string) is
  begin
  end procedure;

  alias my_proc is proc[integer];
  alias my_proc is proc[string];
begin

  process is
    variable i : integer;
    variable s : string(1 to 3);
  begin
    my_proc(i);                         -- OK
    my_proc(s);                         -- OK
  end process;

end architecture;
