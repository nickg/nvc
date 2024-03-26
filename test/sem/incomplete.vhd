entity incomplete is
end entity;

architecture test of incomplete is
    type t1;
    function func1 return t1;           -- Error

    type t2;
    type t3 is access t2;               -- OK

    type t2 is protected
        function func2 return integer;
    end protected;

    type t2 is protected body
        function func2 return integer is
        begin
            return 42;
        end function;
    end protected body;

    shared variable v1 : t2;            -- OK
    constant c1 : integer := v1.func2;  -- OK

    procedure proc1 (variable x : inout t3) is
    begin
        assert x.func2 = 42;            -- OK
    end procedure;

    type t4 is access t1(1 to 2);       -- Error
    type t5 is array (natural range <>) of t1;  -- Error

    type t6 is record
        f : t1;                         -- Error
    end record;

    constant c2 : integer := t1'length;  -- Error
    constant c3 : integer := t1'left;    -- Error

    function func3 (x : t1) return integer;  -- Error

    type t7 is access t1;

    procedure proc2 (variable x : inout t7) is
    begin
        x.all(1 to 3) := x.all(3 to 5);  -- Error
    end procedure;
begin
    b: block is
        generic ( g : t1 := 1 );             -- Error
        port ( p : t1 := 2 );                -- Error
    begin
    end block;
end architecture;
