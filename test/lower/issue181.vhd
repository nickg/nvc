entity issue181 is
end entity;

architecture a of issue181 is
  procedure proc is
    variable str : string(1 to 0);      -- Error here
    variable bitv : bit_vector(1 to 0); -- Works
  begin
      -- Index check here should be elided
  end procedure;
begin
  main : process
  begin
    proc;
    wait;
  end process;
end architecture;
