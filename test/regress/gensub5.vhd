entity gensub5 is
end entity;

architecture test of gensub5 is
    function f
        generic(type t; function to_string(x : t) return string is <>)
        parameter(value : t ; sfmt : string := "s")
        return string
    is
    begin
        return to_string(value) & sfmt;
    end function ;

    type state_t is (IDLE, CHECKING, FOO, BAR) ;

    function to_string(x : state_t) return string is
    begin
        return state_t'image(x) ;
    end function ;

    function f is new f generic map (t => state_t) ;
begin

    p1: process is
    begin
        assert f(idle) = "idles";
        assert f(foo) = "foos";
        wait;
    end process;

end architecture;
