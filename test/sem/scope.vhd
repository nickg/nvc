package pack1 is
    type my_int1 is range 0 to 10;
end package;

-------------------------------------------------------------------------------

package pack2 is
    type my_int1 is range 0 to 10;
end package;

-------------------------------------------------------------------------------

use work.pack1;
use work.pack2;

entity no_use_clause is
    port (
        a : in pack1.my_int1;           -- OK
        b : out pack2.my_int1 );        -- OK
end entity;

-------------------------------------------------------------------------------

architecture a of no_use_clause is
    type my_int1 is range 10 to 50;
begin

    process is
    begin
        -- Should fail as types have same name but from different packages
        b <= a;
    end process;

    process is
        variable v : pack2.my_int1;
    begin
        b <= v;                         -- OK
    end process;

    process is
        variable v : my_int1;
    begin
        -- Should fail as local my_int1 distinct from pack1.my_int1
        v := a;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack1.all;

entity foo is
    generic ( g : my_int1 );
    port ( p : in my_int1 );
end entity;

-------------------------------------------------------------------------------

architecture a of foo is
    -- Architecture decls exist in same scope as entity so this should
    -- generate an error
    signal g : my_int1;
begin
end architecture;

-------------------------------------------------------------------------------

architecture b of foo is
    -- Should also generate an error
    signal p : my_int1;
begin
end architecture;

-------------------------------------------------------------------------------

architecture c of foo is
begin

    -- This is OK as processes define a new scope
    process is
        variable p : my_int1;
        variable g : my_int1;
    begin
        g := 6;
        p := 2;
        wait for 1 ns;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity overload is
    port (
        SI:     in      bit;
        SO:     out     bit
    );
end ;

architecture behave of overload is
begin
    foo_inst:
        SO <= SI;
end behave;

-------------------------------------------------------------------------------

use work.all;                           -- OK

entity no_use_clause is
    port (
        a : in pack1.my_int1;           -- OK
        b : out my_int1 );              -- Error
end entity;

-------------------------------------------------------------------------------

package pack3 is
    type my_enum is (E1, E2, E3);
end package;

-------------------------------------------------------------------------------

use work.pack3.all;

package pack4 is
    type my_enum_array is array (integer range <>) of my_enum;
end package;

-------------------------------------------------------------------------------

use work.pack4.all;

architecture a of foo is
    signal x : my_enum_array(1 to 3);           -- OK
    signal y : my_enum_array(1 to 3) := (others => E1);
              -- Error: E1 not visible
begin
end architecture;

-------------------------------------------------------------------------------

package pack5 is
    function func1(x : integer) return boolean;
    function func2(x : integer) return boolean;
    function "and"(x, y : integer) return boolean;
end package;

-------------------------------------------------------------------------------

use work.pack5.func1;

architecture a2 of foo is
begin

    process is
    begin
        assert func1(4);                -- OK
        assert func2(5);                -- Error
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack5.not_here;                -- Error

architecture a3 of foo is
begin
end architecture;

-------------------------------------------------------------------------------

entity bar is
end entity;

architecture a4 of bar is
begin

    process is
        use work.pack1.all;
        variable x : my_int1;           -- OK
    begin
        x := 5;
    end process;

    process is
        variable x : my_int1;           -- Error
    begin
    end process;

    b: block is
        use work.pack1;
        signal x : pack1.my_int1;           -- OK
    begin
    end block;

end architecture;

-------------------------------------------------------------------------------

use work.pack5."and";

architecture a5 of bar is
begin
    process is
    begin
        assert 1 and 2;                 -- OK
        assert work.pack5."and"(1, 2);  -- OK
        assert pack5."and"(1, 2);       -- OK
    end process;
end architecture;

-------------------------------------------------------------------------------

package pack6 is
    component bar is
    end component;
end package;

-------------------------------------------------------------------------------

use work.pack6.all;

architecture a6 of bar is
begin
    process is
    begin
        report bar'path_name;           -- OK (references entity)
    end process;
end architecture;

-------------------------------------------------------------------------------

use foo.bar.all;                        -- Error

architecture a7 of bar is
begin
end architecture;

-------------------------------------------------------------------------------

package pack7 is
    function foo(x : in integer) return boolean;
    function foo(y : in real) return boolean;
end package;

-------------------------------------------------------------------------------

use work.pack7.foo;

architecture issue62 of bar is
begin

    process is
    begin
        assert foo(integer'(1));        -- OK
        assert foo(real'(1.6));         -- OK
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.all;
use work.pack1.all;

architecture issue63 of bar is
    signal x : my_int1;                 -- OK
begin
end architecture;

-------------------------------------------------------------------------------

package pack8 is
    function min(x, y : in integer) return integer;
end package;

-------------------------------------------------------------------------------

use work.pack8.all;                     -- OK

architecture unit_decl_crash of bar is
begin
    process is
        variable x : integer := min(1, 2);  -- OK
    begin
    end process;
end architecture;

-------------------------------------------------------------------------------

architecture labels of bar is
    signal mySignalVector:  bit_vector (7 downto 0);
    signal myOtherSignal:   bit := '1';
begin
    process
    begin
L1:
        for i in 0 to 9 loop
            for i in 0 to 7 loop
                mySignalVector(i) <= myOtherSignal;
                report "outer loop i = " & integer'image(L1.i);
                report "inner loop i = " & integer'image(i);
                report integer'image(L1.x);  -- Error
            end loop;
        end loop;
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

architecture more_labels of bar is
begin
    p1: process is
        variable x : boolean;
    begin
        p1.x := true;                   -- OK
        for x in 1 to 10 loop
            p1.x := false;              -- OK
        end loop;
    end process;

    b1: block is
        constant x : integer := 2;
    begin
        process is
            variable x : boolean;
        begin
            x := true;                  -- OK
            assert b1.x = 2;            -- OK
        end process;
    end block;

end architecture;
