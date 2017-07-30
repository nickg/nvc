package pkg is
  type type_t is (a,b,c,d);
  function type_t_image(data : type_t) return string;
end package;

package body pkg is
  function type_t_image(data : type_t) return string is
  begin
    return type_t'image(data);
  end;
end;

use work.pkg.all;

entity bug is
end entity;

architecture arch of bug is
begin
  main : process
  begin
    assert type_t_image(a) = "a";
    assert type_t_image(b) = "b";
    assert type_t_image(c) = "c";
    assert type_t_image(d) = "d";
    report "Success";
    wait;
  end process;
end;
