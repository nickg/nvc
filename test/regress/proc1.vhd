entity proc1 is
end entity;

architecture test of proc1 is

    procedure add1(x : in integer; y : out integer) is
    begin
        y := x + 1;
    end procedure;
    
begin

    process is
        variable a, b : integer;
    begin
        a := 2;
        add1(a, b);
        assert b = 3;
        add1(5, b);
        assert b = 6;
        wait;
    end process;

end architecture;
