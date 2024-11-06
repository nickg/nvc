entity tb_simple is
end tb_simple;

architecture tb of tb_simple is
  type my_rec is array (7 downto 0) of bit_vector;
  signal sigA : my_rec(open)(0 downto 0);  -- OK
  signal sigB : my_rec(7 downto 0)(0 downto 0);  -- Error
begin
  sigA(0)(0) <= '0';
end tb;
