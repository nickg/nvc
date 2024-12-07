package p1 is
end p1;

use work.p1.all;                        -- Error

package p1 is
  constant c : natural := 5;
end p1;

use work.p1.all;                        -- Error

entity p1 is
end entity;
