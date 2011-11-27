package p is

    procedure foo(x : in integer; y : out integer);
    
end package;

package body p is

    procedure foo(x : in integer; y : out integer) is
        variable i : integer;
    begin
        y := x + 1;
    end procedure;

end package body;
