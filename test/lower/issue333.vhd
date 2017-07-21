entity issue333 is
end entity;

architecture a of issue333 is

  type line is access string;

  procedure proc(l : inout line) is
  begin
    report integer'image(l'length);
  end procedure;

begin

  main : process is
    variable l : line;
  begin
    l := new string'("1");
    proc(l);
    l := new string'("12"); -- Uncomment to make it work
    wait;
  end process;

end architecture;
