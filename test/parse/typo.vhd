entity typo is
end entity;

architecture test of typo is
    signal areset, reset : bit;
    constant k : bolean;                -- Error

    type rec is record
        foo, bar : integer;
    end record;

    type pt is protected
        procedure one;
        procedure two;
    end protected;

    type pt is protected body
        procedure one is begin end procedure;
        procedure two is begin end procedure;
    end protected body;

begin
    assert rset;                  -- Error

    p1: process is
        function myfunc(x : integer) return integer is
        begin
            return x * 2;
        end function;
    begin
        assert noew = 45 ns;            -- Error
        assert my_func(4) = 8;          -- Error
    end process;

    p2: process is
        variable r : rec;
    begin
        r.frodo := 1;
    end process;

    p3: process is
        variable p : pt;
    begin
        p.onn;                          -- Error
    end process;
end architecture;
