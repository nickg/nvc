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
    signal m : int_array(1 to 3) := ( 1 to 3 => 0 );
    alias a : int_array(2 to 3) is x(2 to 3);
    
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

    process is
        variable b : boolean;
    begin
        b := z = m;                     -- OK
        b := z /= m;                    -- OK
        b := z = y;                     -- Different types
    end process;

    process is
    begin
        x(1 to 3) <= z;
        x(1 to 2) <= z(1 to 2);
        x(x'range) <= (others => 0);
    end process;

    process is
    begin
        a(2) <= 4;                      -- OK
        y(2) <= 1;                      -- OK
    end process;

    process is
        type int2d is array (1 to 10, 1 to 4) of integer;
        variable w : int2d := ( 1 => ( 1, 2, 3, 4 ),
                                2 => ( others => 5 ),
                                others => ( others => 0 ) );
    begin
        w(2, 4) := 6;
        w(6) := 6;                      -- Too few indices
        w(6, 7, 2) := 2;                -- Too many indices
    end process;

    process is
        type letter is (A, B, C);
        type larray is array (letter) of integer;
        variable w : larray;
    begin
        w(A) := 2;                      -- OK
        w(5) := 66;                     -- Wrong index type
    end process;

    process is
        variable n : int_array(1 to 3) := ( 0, 1 => 1, others => 2 );  -- Error
    begin
    end process;

    process is
        variable x : integer;
        constant c : integer := 3;
        variable y : int_array(1 to 3);
    begin
        y := ( 1 => 2, 2 => 3, x => 5 );  -- Error
        y := ( 1 => 2, 2 => 3, c => 5 );  -- OK
    end process;

    process is
        variable x : integer;
        variable y : int_array(3 downto 0);
    begin
        x(1 to 3) := (others => 4);     -- Error
        y(1 to 3) := (others => 4);     -- Error
        assert y = (others => 4);       -- Error
    end process;

    process is
        subtype five_ints is ten_ints(1 to 4);
        variable x : five_ints;
    begin
        x(1 to 3) := (1, 2, 3);         -- OK
        x(2) := 1;                      -- OK
        x(3 downto 1) := (others => '0');  -- Error
        assert x(2) = 5;                -- OK
    end process;       

    process is
        function foo(size: integer) return int_array is
	    subtype rtype is int_array(size-1 downto 0);
            variable result: rtype;
        begin
            assert result(0) = 1;
            return result;
        end;
    begin
    end process;

    process is
        function plus(A, B: int_array) return int_array is
            variable BV, sum: int_array(A'left downto 0);
        begin
            return sum;
        end;
    begin
    end process;

    process is
        subtype int4_t is int_array(1 to 4);
        type foo_t is array (integer'left to 10) of integer;
        variable v : int_array(foo_t'range);
    begin
        assert int4_t'length = 4;
    end process;

end architecture;
