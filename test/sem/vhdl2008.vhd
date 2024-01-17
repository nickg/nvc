entity vhdl2008 is
    generic ( g : integer );
end entity;

architecture test of vhdl2008 is
    signal s : integer;
begin

    process is
        variable x, y : integer;
    begin
        x := 1 when y > 2 else 5;       -- OK
        x := 5 when x;                  -- Error
        x := 1 when x < 1 else false;   -- Error
    end process;

    -- Changes to locally static rules
    process is
        type r is record
            k : bit;
        end record;
        constant c : bit_vector(1 to 3) := "101";
        constant d : r := ( k => '1' );
        variable x : bit;
        variable y : r;
        variable i : integer;
    begin
        case x is
            when c(1) => null;          -- OK
            when d.k => null;           -- OK
            when c(i) => null;          -- Error
            when others => null;
        end case;
    end process;

    -- 'SUBTYPE attribute
    process is
        variable x : integer;
    begin
        x := baz'subtype(4);            -- Error
        report to_string(x'subtype);    -- Error
    end process;

    g1: case g generate                 -- OK
        when 5 =>
            s <= 2;
        when g =>                       -- OK
            s <= 3;
        when s =>                       -- Error
            s <= 5;
    end generate;

    -- Range in aggregate target
    process is
        variable v : integer_vector(1 to 2);
        variable a, b : integer;
    begin
        (1 to 2 => v) := v;             -- OK
        (1 to 2 => a) := v;             -- Error
    end process;

    -- Changes to closely related types
    process is
        variable i : integer_vector(1 to 3);
        variable r : real_vector(1 to 3);
        variable b : boolean_vector(1 to 3);
    begin
        i := integer_vector(i);         -- OK
        r := real_vector(i);            -- OK
        b := boolean_vector(i);         -- Error
    end process;

    -- Changes to globally static attribute rules
    b1: block is
        type t_rec is record
            x : bit_vector;
        end record;
        signal s : t_rec(x(1 to 5));
    begin
        b2: block is
            port ( p : t_rec );
            port map ( s );
        begin
            g1: for i in p.x'range generate  -- OK
            end generate;
            g2: for i in q.x'range generate  -- Error
            end generate;
        end block;
    end block;

    -- Port maps with non-globally-static expressions
    b2: block is
        type t_array is array (natural range <>) of integer;
        signal s : integer;
        function get_array (signal s : integer; constant x : integer) return t_array is
        begin
            return (1 to 3 => s + x);
        end function;
    begin
        sub1: block is
            port ( p1 : t_array );
            port map ( p1 => get_array(s, 5) );  -- Error
        begin
        end block;
    end block;

end architecture;
