package test_pkg is
  type t1 is record
    f1 : bit;
  end record t1;
  subtype t1_sub is t1;
end package;

-------------------------------
library work;
use work.test_pkg.all;

entity test is
  port (
    pin  : in  t1;
    pout : out t1_sub
  );
end entity;

architecture test_arch of test is
begin
  pout.f1 <= pin.f1;
end architecture;

-------------------------------
library work;
use work.test_pkg.all;

entity sub is
end entity;

architecture sub_arch of sub is
  signal a : t1_sub;
  signal b : t1_sub := (f1 => '0');
begin

  cmp: entity work.test
    port map (
      pin => b,
      pout => a
    );

end architecture;
