package body dunno is
    type foo is array (bar) of character;  -- Error
    type goo is array (bar range '1' to '2') of character;  -- Error (suppressed)

    function func(x : sdff) return boolean is
    begin
        return false;
    end function;

    function func(x : sghbbx) return boolean is
    begin
        return true;
    end function;

    procedure proc is
    begin
        std.nothere.bang(4);            -- Error
    end procedure;

    procedure proc2 is
    begin
        for foo in integer range blah blah loop  -- Error
        end loop;
    end procedure;

    type badunits is range 1 to 30
        units
            blah;
        end units spork;

    type badrecord is record
        x : integer;
    end record wrong;

    function func2 return integer is
    begin
        return 1;
    end procedure;                      -- Error

    procedure proc3 is
    begin
    end function;                       -- Error

    type other is protected
    end protected wrong;                -- Error

    subtype weird is bit_vector 1 to 3; -- Error

    procedure proc4 is
        type ft is file of integer;
        file f : ft;
    begin
        f(0) := 1;                      -- Error
    end procedure;

end package body;
