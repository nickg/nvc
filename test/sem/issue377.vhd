package p1 is
    type int_vector is array (integer range <>) of integer;
end package;

use work.p1.all;

package p2 is
    function "="(x, y : int_vector) return boolean;
end package;

entity issue377 is
end entity;

use work.p1.all;
use work.p2.all;

architecture test1 of issue377 is
    signal x, y : int_vector(3 downto 0);
begin
    assert x = y;                       -- Error
end architecture;

use work.p2.all;
use work.p1.all;

architecture test2 of issue377 is
    signal x, y : int_vector(3 downto 0);
begin
    assert x = y;                       -- Error
end architecture;
