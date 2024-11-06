entity issue1057 is
end issue1057;

architecture tb of issue1057 is
  type my_rec is array (7 downto 0) of bit_vector;
  signal sigA : my_rec(open)(0 downto 0);  -- OK
begin
  sigA(0)(0) <= '0';
end tb;
