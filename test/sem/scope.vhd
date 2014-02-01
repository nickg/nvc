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
        a : in pack1.my_int1;
        b : out pack2.my_int1 );
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

use work.all;

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
