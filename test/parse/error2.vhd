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
end package body;
