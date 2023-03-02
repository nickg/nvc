entity e is
end entity;

package p is
    function f(x : boolean) return boolean;
end package;

package body p is
    function f(x : boolean) return boolean is
    begin
        assert false report "do not call this";
        return false;
    end function;
end package body;

use work.p.all;

architecture test of e is
    function f1(x : boolean) return boolean is
    begin
        return x and f(x);
    end function;

    function f2(x, y : boolean) return boolean is
    begin
        return x and y;
    end function;

    function f3(x, y, z : boolean) return boolean is
    begin
        return x and (y or z);
    end function;

    function f4(x : boolean) return boolean is
        variable y : boolean;
    begin
        y := false;
        return y and f(x);
    end function;

    function f5(x : boolean) return boolean is
        variable y : boolean;
    begin
        y := false;
        return y nor f(x);
    end function;

    function f6(x : boolean) return boolean is
    begin
        return x or f(x);
    end function;

    function f7(x : boolean) return boolean is
    begin
        return x nor f(x);
    end function;
begin

end architecture;
