package pkg is
  type stuff is record
    req: natural;
    resp: natural;
  end record;

  view stuff_controller of stuff is
    req: in;
    resp: out;
  end view;
  alias stuff_target is stuff_controller'converse;

end package;

use work.pkg.all;

entity foo is
end entity;

architecture test of foo is
  procedure baz (
    signal x: view stuff_controller -- <--- not correct but results in nvc crash
    --signal x: view stuff_target -- <--- correct and works ok
  ) is begin
    x.req <= 4;
  end procedure;

  signal data: stuff;
begin
  baz(data);
end;
