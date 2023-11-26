entity bug is end entity;

architecture mixed of bug is
begin

log: process is
  variable x : real;
  variable y : time;
begin
  x := 2.0;
  y := abs(x*5.0)**2 * 1.0 ns;
end process;

end architecture;
