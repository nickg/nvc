library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub is
    port (
        x : in  std_logic_vector(7 downto 0);
        y : out std_logic_vector(7 downto 0) );
end entity;

architecture test of sub is
    signal ctr  : unsigned(3 to 18) := (others => '0');
    signal ctr2 : unsigned(15 downto 0) := (others => '0');
begin

    y <= x after 5 ns;

    ctr <= ctr + 1 after 20 ns;

    ctr2 <= ctr2 + 1 after 20 ns;

end architecture;

-------------------------------------------------------------------------------

package p is
    signal s : bit;
end package;

-------------------------------------------------------------------------------

entity wave3 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

use work.p.all;

architecture test of wave3 is
    signal x : std_logic_vector(7 downto 0) := X"AA";
    signal y : std_logic_vector(7 downto 0);
    signal z : std_logic := 'U';
    signal o : std_logic := '0';
    signal b : boolean;
    signal m : string(1 to 3);
    signal p : bit_vector(1 to 3);
    signal q : bit_vector(3 downto 1);
    signal t : delay_length;

    type state is (INIT, ONE, TWO);
    signal s : state;

    subtype state_sub is state range ONE to TWO;
    signal s2 : state_sub;
begin

    x <= not x after 50 ns;

    z <= 'X' after 100 ns,      -- Appears same as 'U'
         'H' after 200 ns,
         'Z' after 300 ns,
         'L' after 400 ns,
         '-' after 500 ns,
         '1' after 600 ns;

    a_block: block is
        signal i : natural;    -- No integer type in FST?
        signal c : character;
    begin
        i <= i + 1 after 20 ns;
        c <= m((i rem 3) + 1);
    end block;

    m <= "abc";

    work.p.s <= '1';

    b <= true;

    s <= ONE after 60 ns, TWO after 150 ns;

    sub_i: entity work.sub
        port map ( x, y );

    gen: for i in 1 to 3 generate
        signal g : integer;
    begin
    end generate;

    p(1) <= '1';
    p(2) <= '1';
    p(3) <= '0';

    q(1) <= '1';
    q(2) <= '1';
    q(3) <= '0';

    t <= 20 us after 1 ps;

    s2 <= TWO after 4 ns;

end architecture;
