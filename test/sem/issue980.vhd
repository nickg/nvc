package issue980 is
    type t_enum is (a, b, c);
    subtype t_sub is t_enum range a to b;

    function foo return t_sub;
    procedure bar (x : t_sub);
end package;

package body issue980 is
    function foo return t_enum is       -- Error
    begin
    end function;

    procedure bar (x : t_enum) is       -- Error
    begin
    end procedure;
end package body;
