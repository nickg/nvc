package pkg is
  type enum_t is (enum0, enum1);
end package;

entity bug is
end entity;

use work.pkg.enum1;
use work.pkg;
use work.pkg.all;

architecture a of bug is
  constant enum2 : work.pkg.enum_t := work.pkg.enum0;
  constant enum3 : pkg.enum_t := pkg.enum1;
begin

  main : process is
  begin
    assert enum2 = work.pkg.enum0;
    assert enum3 = pkg.enum1;
    wait;
  end process;
end;
