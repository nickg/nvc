package p is

    type r1 is record                   -- OK
        x : integer;
        y : integer;
    end record;

    type r2 is record                   -- Error
        x, x : integer;
    end record;

    type r3 is record                   -- Error
        x : r3;
    end record;

    type r4 is record
        x, y, z : integer;
    end record;

    type r5 is record
        x : r1;
        y : integer;
    end record;

    type r1_vec is array (integer range <>) of r1;

    type r6 is record
        x : r1_vec;                     -- Error
    end record;

end package;

package body p is

    procedure p1 is
        variable v1 : r1 := (1, 2);     -- OK
        variable v2 : r4 := (1, 2);        -- Error
        variable v3 : r1 := (1, v1);       -- Error
        variable v4 : r1 := (x => 1, y => 2);  -- OK
        variable v5 : r1 := (x => 1);   -- Error
        variable v6 : r1 := (x => 1, y => 2, q => 1);  -- Error
        variable v7 : r1 := (x => 1, y => v1);  -- Error
        variable v8 : r1 := (others => 9);  -- OK
        variable v9 : r1 := (x => 1, others => 2);
        variable v10 : r1 := (x => 1, x => 2, y => 3);  -- Error
        variable v11 : r1 := (1, x => 4, y => 2);  -- Error
        variable v12 : r1 := (1, y => 4);  -- OK
        variable v13 : r1;
    begin
    end procedure;

    procedure p2 is
        variable v1 : r1;
        variable v2 : r5;
    begin
        v1.x := 2;
        v1.y := v1.x + 5;
        v2.x.x := 3;
    end procedure;

    procedure p3 is
        variable a1 : r1_vec;           -- Error
    begin
    end procedure;

    procedure p4 is
        variable a2 : r1_vec(0 to 3);   -- OK
    begin
        a2(2).x := 5;                   -- OK
        a2(1).f := 2;                   -- Error
        a2(0).x := a2(1).y;             -- OK
    end procedure;

    procedure p5 is
        subtype r1_sub is r1;           -- OK
        variable a : r1_sub;            -- OK
    begin
        a.x := 5;                       -- OK
        a.y := a.x + 2;                 -- OK
        a.z := 2;                       -- Error
    end procedure;

    procedure p6 is
        subtype r1_bad is r1(1 to 3);   -- Error
    begin
    end procedure;

    procedure p7 is
        type rec is record
            vec : bit_vector(1 to 3);
        end record;
        variable a : rec;
    begin
        assert a.vec'length = 3;        -- OK
    end procedure;

    procedure p8 is
        function make_r1 return r1 is
        begin
            return (x => 1, y => 2);    -- OK
        end function;
    begin
        assert make_r1.x = 1;           -- OK
        assert make_r1.z = 2;           -- Error
    end procedure;

    type int_file is file of integer;
    type r7 is record
        a : int_file;
    end record;

    type r8 is record
        a : integer range 1 to 10;
    end record;

    procedure p9 is
        variable x : r8;                -- OK
    begin
    end procedure;

    procedure p10 is
        variable x : r8 := (ack => '1');  -- Error
    begin
    end procedure;

    type line is access string;

    -- Copied from incorrect code in std.textio
    procedure read (l     : inout line;
                    value : out time;
                    good  : out boolean ) is
        type unit_spec_t is record
            name   : string(1 to 3);
            length : positive;
            unit   : time;
        end record;

        type unit_map_t is array (natural range <>) of unit_spec_t;

        constant unit_map : unit_spec_t := (
            ( "fs ", 2, fs ) );

        variable scale, len : integer;
        variable scale_good : boolean;
    begin
        good := false;
        if not scale_good then
            return;
        end if;
        for i in 0 to 0 loop
            len := unit_map(i).length;  -- Error
            if l'length > len
                and l.all(1 to len) = unit_map(i).name(1 to len)
            then
                value := scale * unit_map(i).unit;
                good := true;
            end if;
        end loop;
    end procedure;

    procedure aggregates is
        variable r : r1;
    begin
        r := (1 => 1);                  -- Error
        r := (1, 2, 3);                 -- Error
        r := (1, 2, others => 3);       -- Error
        r := (x to y => 4);             -- Error
        r := (x to z => 2);             -- Error
    end procedure;

    procedure test1 is
        variable r : foo;               -- Error
    begin
        r.baz := 1;                     -- Error
    end procedure;

    procedure test2 is
        procedure sub (x : integer) is
        begin
        end procedure;
    begin
        sub(x.z => 2);                  -- Error
    end procedure;

    procedure test3 is
        variable r : r1 := r1'left;     -- Error
    begin
    end procedure;

end package body;
