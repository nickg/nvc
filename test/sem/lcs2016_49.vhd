entity e is
end entity;

architecture a of e is
    function func1 generic (type t) (x, y : t) return t is
    begin
        return x;
    end function;

    procedure proc1 generic (type t) (x : t) is
    begin
    end procedure;
begin

    process is
    begin
        assert func1 generic map (integer) parameter map (1, 2) = 1;  -- OK
        proc1 generic map (bit) ('1');  -- OK
        proc1 generic map (integer) parameter map (2);  -- OK
        assert proc1 generic map (integer) (1);  -- Error
        proc1 generic map (bit) (1);    -- Error
        proc1 generic map (func1) (1);  -- Error
        wait;
    end process;

end architecture;
