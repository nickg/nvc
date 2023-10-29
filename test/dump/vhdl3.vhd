package vhdl3 is
    generic (g : integer);
    constant c1 : bit_vector(1 to g);
end package;

package body vhdl3 is
    constant c1 : bit_vector(1 to g) := (others => '1');
    constant c2 : integer := << constant .uut.x : integer >>;
end package body;

package vhdl3_i is new work.vhdl3;
