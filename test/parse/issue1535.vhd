entity sub is
end entity;

architecture test of sub is
begin
end architecture;

library work;
use work.sub;

entity issue1535 is
end entity;

architecture rtl of issue1535 is
begin
  i_test : work.sub;
end architecture;
