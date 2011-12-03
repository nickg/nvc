package p is

    procedure foo(x : in integer; y : out integer);
    
end package;

package body p is

    procedure foo(x : in integer; y : out integer) is
        variable i : integer;
    begin
        y := x + 1;
    end procedure;

    procedure bar(x : in integer; signal y : out integer) is
    begin
        y <= x + 1;
    end procedure;

    procedure yam is
    begin
        return;                         -- OK
        return 5;                       -- Error
    end procedure;
    
end package body;
