entity e is
end entity;

architecture test of e is

    function f(x : string) return boolean is
    begin
        return x = "";
    end function;

begin
end architecture;
