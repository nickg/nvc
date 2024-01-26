--
-- Grab bag of miscellaneous VHDL-2008 syntax
--

entity vhdl2008 is
end entity;

package genpack is
    generic ( x : integer := 5; y : boolean );  -- OK
    generic map ( x => 5, y => false );         -- OK

    constant c : bit_vector(1 to x) := (1 to x => '1');
end package;

package genpack2 is
    generic ( x : integer := 5; y : boolean );  -- OK
    function add_x_if_y ( arg : integer ) return integer;
end package;

package body genpack2 is
    function add_x_if_y ( arg : integer ) return integer is
    begin
        if y then
            return arg + x;
        else
            return arg;
        end if;
    end function;
end package body;

package primary_genpack2 is new work.genpack2 generic map (4, false);  -- OK

architecture test of vhdl2008 is
    type my_utype is (a, b, c);
    type my_utype_vector is array (natural range <>) of my_utype;

    function resolved (s : my_utype_vector) return my_utype;

    subtype my_type is resolved my_utype;
    subtype my_type_vector is (resolved) my_utype_vector;  -- OK

    type my_logical_vec is array (natural range <>) of bit;

    type my_bool is (true, false);
    package my_genpack2 is new work.genpack2 generic map (1, true);  -- OK
begin

    process is
        variable b : bit;
        variable v : my_logical_vec(1 to 3);
    begin
        b := or v;                      -- OK
        if or v = '1' then end if;      -- OK
        b := and v;                     -- OK
        b := xor v;                     -- OK
        b := xnor v;                    -- OK
        b := nand v;                    -- OK
        b := nor v;                     -- OK
    end process;

    process is
        variable b : bit;
        variable v : my_logical_vec(1 to 3);
    begin
        b := b ?= '1';                  -- OK
        b := b ?/= '1';                 -- OK
        b := b ?< '0';                  -- OK
        b := b ?> '0';                  -- OK
        b := b ?<= '1';                 -- OK
        b := b ?>= '1';                 -- OK
        b := v ?= "101";                -- OK
        b := v ?/= "111";               -- OK
    end process;

    process is
        variable b : bit;
        variable i : integer;
        function "??"(x : integer) return boolean;
    begin
        if b then end if;               -- OK
        if b xor '1' then end if;       -- OK
        while b and '1' loop end loop;  -- OK
        if i + 1 then end if;           -- OK
        if now + 1 ns then end if;      -- Error
        while true loop
            exit when b or '1';         -- OK
            next when b or '1';         -- OK
        end loop;
        wait until b xor '0';           -- OK
        assert b nor '1';               -- OK
        assert ?? 1;                    -- OK
        assert b and b and (not b);     -- OK
    end process;

    /* This is a comment */

    /* Comments /* do not nest */

    process is
        variable x, y : integer;
    begin
        x := 1 when y > 2 else 5;       -- OK
    end process;

    process is
        variable x : string(7 downto 0);
    begin
        x := 8x"0";                     -- OK
        x := 6x"a";                     -- OK
        x := 4x"4";                     -- OK
        x := 2x"4";                     -- Error
        x := 0x"5";                     -- Error
        x := 18x"383fe";                -- OK
        x := 0b"0000";                  -- OK
        x := d"5";                      -- OK
        x := 5d"25";                    -- OK
        x := 120d"83298148949012041209428481024019511";  -- Error
        x := uo"5";                     -- OK
        x := 5sb"11";                   -- OK
        x := 2sb"1111110";              -- OK
        x := 2sb"10110101";             -- Error
        x := 4x"0f";                    -- OK
        x := Uo"2C";                    -- OK
        x := d"C4";                     -- Error
        x := 8x"-";                     -- OK
        x := 12d"13";                   -- OK
        x := 3d"13";                    -- Error
    end process;

    b2: block is
        signal s : integer;
    begin

        process is
        begin
            s <= 1 when s < 0 else 5;   -- OK
        end process;

    end block;

    process is
        type int_vec2 is array (natural range <>) of integer_vector;  -- OK
        constant a : int_vec2(1 to 3)(1 to 2) := (  -- OK
            (1, 2), (3, 4), (5, 6) );
    begin
        assert a(1)(1) = 1;             -- OK
    end process;

    b3: block is
        signal s : integer;
    begin
        process is
        begin
            s <= force 1;               -- OK
            s <= force out 1;           -- OK
            s <= force in 2;            -- OK
            s <= release;               -- OK
            s <= release out;           -- OK
        end process;
    end block;

    process is
        variable x : bit_vector(1 to 3);
    begin
        case? x is                      -- OK
            when "010" => null;
            when others => null;
        end case?;
        case? x is
            when others => null;
        end case;                       -- Error
        case x is
            when others => null;
        end case ?;                     -- Error
    end process;

    b4: block is
        procedure foo (x : integer_vector; y : integer) is
            variable a : x'subtype;     -- OK
            variable b : integer'subtype;  -- OK
            variable c : b4'subtype;    -- Error
            variable d : x'element;     -- OK
            variable e : y'element;     -- Error
            variable f : b4'element;    -- Error
        begin
        end procedure;
    begin
    end block;

    b5: block is
        function gen1 generic (n : integer) (x : integer) return integer is
        begin                           -- OK
            return 1;
        end function;
        function gen2 generic (n : integer)  -- OK
            parameter (x : integer) return integer;
        function gen3 generic (type t; p : t) (x : t) return integer;  -- Ok

        function my_gen1 is new gen1 generic map (5);  -- OK
    begin
    end block;

    b6: block is
        constant c1 : string := to_string(100);  -- OK
    begin
    end block;

    b7: block is
        signal s : integer;
        signal b : bit;
    begin
        s <= 1 when b else 2;           -- OK
        s <= 2 when '1' else 6;         -- OK
        process is
            variable v : integer;
        begin
            v := 1 when b else 5;       -- OK
            s <= 5 when b else 7;       -- OK
        end process;
    end block;

    g1: if g1a: true generate           -- OK
    elsif g2: false generate
    begin
    end g2;
    else generate
    end generate;

    g1: if true generate
    end g1;                             -- Error
    else foo: generate
    end bar;                            -- Error
    end generate;

    g2: case integer'(5) generate       -- OK
        when 1 =>
            signal s : bit;
        begin
            s <= '1';
        end;
        when 2 => begin end;
        when others => begin end;
    end generate;

    g3: case integer'(5) generate       -- OK
        when foo: 1 => begin end;
        when 2 => begin end;
        when bar: others => begin end;
    end generate g5;                    -- Error

    /* a delimited comment
    -- Check that write burst was received correctly
    */

    b8: block is
        signal s : bit;
    begin
        g: block (s) is         -- OK
        begin
        end block;
    end block;

    b9: block is
        constant x : bit_vector(3 downto 0) := 4sx"";  -- Error
        constant y : bit_vector(0 downto 0) := 4x"";  -- OK
    begin
    end block;

    b10: block is
        type t_rec is record
            x : integer_vector;
        end record;
        type t_rec_array is array (natural range <>) of t_rec;
        function f return t_rec_array;
        constant c1 : t_rec_array := f;  -- OK
        constant c2 : c1'subtype := c1;  -- OK
    begin
    end block;

    b11: block is
        generic ( g : in integer );
        generic map ( g => inertial 5 + 1 );  -- Error
        port ( x : in integer );
        port map ( x => inertial 3 + 4 );  -- OK

        procedure proc ( signal x : integer ) is
        begin
        end procedure;
    begin
        proc(x => inertial x);          -- Error
    end block;
end architecture;
