-- library foo
package pack is
end package;

entity sub2 is
end entity;

architecture a of sub2 is
begin
end architecture;

use work.pack.all;

entity sub1 is
end entity;

architecture a of sub1 is
    component sub2 is
    end component;
begin

    sub_i: component sub2;

end architecture;

-- library bar
library foo;

entity binding1 is
end entity;

architecture test of binding1 is
    component sub1 is
    end component;
begin

    sub_i: component sub1;

end architecture;
