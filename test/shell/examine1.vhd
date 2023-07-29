entity examine1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of examine1 is
    signal x : integer := 5;
    signal y : integer;
    signal a : std_logic := '1';
    signal b : std_logic_vector(3 downto 0) := "01XU";
    signal s : string(1 to 5) := "hello";
    signal ss : string(1 to 2) := (1 => 'a', others => nul);

    type t is (foo, bar, baz);
    type t_vec is array (natural range <>) of t;

    signal v : t_vec(1 to 3) := (foo, bar, baz);

    signal p : time := 42 ns;
begin

end architecture;
