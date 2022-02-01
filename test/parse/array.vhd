package p is

    type int_array is array (integer range <>) of integer;

    type ten_ints is array (1 to 10) of integer;

    type chars is (A, B, C);
    type char_counts is array (chars) of integer;

    type two_d is array (1 to 3, 4 to 6) of integer;

    type ab_chars is array (chars range A to B) of integer;

    type it is array (integer range ten_ints'range) of bit;

end package;

entity e is end entity;

use work.p.all;

architecture a of e is
    signal x : int_array(1 to 5);
    signal y : ten_ints;
    signal z : int_array(1 to 3) := ( 0, 1, 2 );
    signal n : int_array(1 to 3) := ( 0, 1 => 1, others => 2 );
    signal m : int_array(1 to 3) := ( 1 to 3 => 0 );
    signal c : char_counts;
    signal t : two_d;
    signal u : ten_ints := ( 1 | 2 | 3 => 4, others => 2);
    signal v : ten_ints := ( 1 ! 2 ! 3 => 4, others => 2);
begin

    process is
        variable k : int_array(1 to 5);
    begin
        x(0) <= 1;
        y(2) <= n(2);
        y(3)(5) <= n(2)(1);
        x(1 to 3) <= z(1 to 3);
        k := (x'range => 5);
        k := (x'reverse_range => 3);
    end process;

end architecture;
