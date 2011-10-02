package p is

    type int_array is array (integer range <>) of integer;

    type ten_ints is array (1 to 10) of integer;
    
end package;

entity e is
end entity;

use work.p.all;

architecture a of e is
    -- All these declarations are OK
    signal x : int_array(1 to 5);
    signal y : ten_ints;
    signal z : int_array(1 to 3) := ( 0, 1, 2 );
    signal n : int_array(1 to 3) := ( 0, 1 => 1, others => 2 );
    signal m : int_array(1 to 3) := ( 1 to 3 => 0 );
begin

    process is
        -- Positional elements cannot follow named
        variable e : int_array(1 to 2) := (
            0 => 1, 2 );
    begin
    end process;

    process is
        -- Others element must be last
        variable e : ten_ints := ( others => 5, 1 => 2 );
    begin
    end process;

    process is
        -- Only one others element
        variable e : ten_ints := ( others => 5, others => 2 );
    begin
    end process;

    process is
        -- Single element aggregates must be named
        variable a : int_array(0 to 0) := ( 0 => 1 );
        variable b : int_array(0 to 0) := ( 1 );  -- Error
    begin
    end process;

    process is
        variable a : integer;
    begin
        x(0) <= 1;                      -- OK
        x <= ( others => 2 );           -- OK
        x <= 1;                         -- RHS not array
        a := x(0);                      -- OK
        a := x;                         -- LHS not array
    end process;

end architecture;
