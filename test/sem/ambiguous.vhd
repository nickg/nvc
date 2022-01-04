entity e is
end entity;

architecture a of e is
    type foo is (a, b, c);
    type bar is (a, b, c);

    signal x : foo := a;
    signal y : bar := b;
begin

    process is
    begin
        x <= c;
        y <= a;
    end process;

    process is
    begin
        x <= foo'(a);
        y <= bar'(a);
    end process;

    p3: process is
        type baz is (a, b, c, d);
        variable z : baz := b;
    begin
        z := d;                         -- OK
        z := a;                         -- OK
        x <= a;                         -- OK
    end process;

    process is
    begin
        x <= bar'(c);                   -- Error!
    end process;

    process is
        type small is range 10 downto -5;
        variable z : small := -5;
        variable a : boolean;
    begin
        a := z = -5;                    -- OK
        a := -5 = z;                    -- OK
    end process;

    process is
        variable a : bit_vector(3 downto 0);
        variable x : character;
        variable b : boolean;
    begin
        b := x = '1';                   -- OK
        b := '1' = x;                   -- OK
        b := a = ('0', '1', '0', '1');  -- OK
        b := ('0', '1', '0', '1') = a;  -- OK
        b := ('0', '1') = ('0', '1');   -- Error
    end process;

    process is
        subtype some_foo is foo range a to b;  -- OK
        subtype less_foo is some_foo range a to a;
        subtype all_foo is foo;
        variable f : some_foo;
        variable g : all_foo;
        variable h : less_foo;
    begin
        f := a;                         -- OK
        f := c;                         -- OK at semantic check
        g := f;                         -- OK
        g := h;                         -- OK
    end process;

    process is
        type weird is ( '¢', '¦' );
        variable x : weird;
        variable y : character;
    begin
        x := '¢';                       -- OK
        y := '¢';                       -- OK
        report "foo¥bar";               -- OK
    end process;

    process is
        type t is (false, true);
    begin
        for i in false to false loop    -- Error
        end loop;
    end process;

    process is
        function now return integer;
    begin
        for i in now to now loop    -- Error
        end loop;
    end process;

    process is
        function false return integer is
        begin
            return 1;
        end function;
    begin
        for i in false to false loop    -- Error
        end loop;
    end process;

    process is
        function "="(a, b : foo) return boolean is
        begin
            return false;
        end function;

        variable x, y : foo;
    begin
        assert x = y;                   -- OK
    end process;

end architecture;

package pack is
    type my_int is range 1 to 10;
end package;

use work.pack.all;

package pack2 is
    function "<"(a, b: my_int) return boolean;
end package;

use work.pack2.all;
use work.pack.all;

architecture a2 of e is
    function ">"(a, b: my_int) return boolean;
begin

    process is
        variable x, y : my_int;
    begin
        assert x > y;                   -- OK
        assert x < y;                   -- Error
    end process;

    process is
        function uniform (a, b : real) return real;
        type disttype is (none, uniform, other);
        variable v : disttype;
        variable r : real;
    begin
        case v is
            when none | uniform => r := uniform(1.0, 2.0);  -- OK
            when others => r := 0.0;
        end case;
    end process;

end architecture;

architecture a3 of e is
    type unsigned is array (natural range <>) of bit;

    function "*"(a, b : unsigned) return bit_vector;
    function "*"(a, b : bit_vector) return bit_vector;
    function "*"(a, b : unsigned) return unsigned;

    function "+"(a, b : unsigned) return bit_vector;
    function "+"(a, b : bit_vector) return bit_vector;
    function "+"(a, b : unsigned) return unsigned;

    signal x, y, z : bit_vector(7 downto 0);
begin

    x <= unsigned(y) * unsigned(z) + unsigned(z);

end architecture;

-- Test case reduced from Altera model
architecture a4 of e is
    function resolved (x : bit_vector) return bit;

    subtype rbit is resolved bit;

    type rbit_vector is array (natural range <>) of rbit;

    function "and" (x, y : rbit_vector) return rbit_vector;

    signal mdio_wr  : rbit;
    signal reg_addr : rbit_vector(15 downto 0);
begin

    process is
    begin
        assert ((X"0000" & mdio_wr) and reg_addr) /= X"0000";
    end process;

end architecture;

architecture issue61 of e is
    type ubit_vector is array (natural range <>) of bit;
begin
    process is
        variable x: bit_vector(4 downto 0);
        variable y: ubit_vector(6 downto 0);
    begin
        y := ubit_vector(x & ('0' & '1'));
        y := ubit_vector((x & '0') & '1');
        y := ubit_vector(x & '0' & '1');
        wait;
    end process;
end architecture;

architecture cassign of e is
    function "="(x, y : bit) return bit;
    signal x, y, z : bit;
begin
    x <= '1' when y = z else '0';       -- OK
end architecture;

architecture expect_fail of e is
    type t is (C, B, A);
    type t_vec is array (1 to 2) of t;
    -- Type of aggregate must be determinable from the context
    constant x : boolean := (A, A) < (C, C);  -- Error
begin
end architecture;

-- -*- coding: latin-1; -*-
