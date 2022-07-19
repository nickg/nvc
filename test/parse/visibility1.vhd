package p1 is
    constant k : integer := 1;
    constant j : integer := 5;
    type t is (FOO, BAR);
end package;

package p2 is
    constant k : integer := 2;
    constant j : integer := 7;
    type t is (BAR, BAZ);
end package;

use work.p1.all;
use work.p2.all;

package p3 is
    constant x : integer := k;          -- Error
    constant j : integer := 2;          -- OK
    constant y : integer := j;          -- OK
    constant z : t := FOO;              -- Error
    constant b : boolean := BAR;        -- Error
end package;
