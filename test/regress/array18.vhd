package pack1 is
    constant k : natural;
end package;

package body pack1 is
    constant k : natural := 4;
end package body;

-------------------------------------------------------------------------------

use work.pack1.all;

package pack2 is
    type p is protected
        procedure inc;
        impure function get return natural;
    end protected;

    shared variable c : p;

    impure function f (x : integer) return integer;
    type t is array (1 to k + 1) of bit_vector(f(k) downto 0);
end package;

package body pack2 is
    type p is protected body
        variable v : natural;
        procedure inc is
        begin
            v := v + 1;
        end procedure;
        impure function get return natural is
        begin
            return v;
        end function;
    end protected body;

    impure function f (x : integer) return integer is
    begin
        report "F called!";
        c.inc;
        return x - 1;
    end function;
end package body;

-------------------------------------------------------------------------------

entity array18 is
end entity;

use work.pack2.all;

architecture test of array18 is
    signal s : t;
begin

    p1: process is
        variable v : integer := 5;
        variable v2 : t;
        constant c2 : t := (others => (others => '0'));
    begin
        v2(v)(1) := '1';
        wait for 1 ns;
        s(v)(2) <= '1';
        assert c.get = 1;
        wait;
    end process;
end architecture;
