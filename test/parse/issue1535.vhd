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
  i_test : work.sub;                    -- Error
end architecture;

package pack is
    procedure proc;
end package;

use work.pack;

architecture pcall of issue1535 is
begin
    x: pack.proc;                       -- OK
    y : work.pack;                      -- Error
end architecture;
