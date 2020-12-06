package p is

    procedure foo(x : in integer; y : out integer);

    type text is access string;

end package;

package body p is

    procedure foo(x : in integer; y : out integer) is
        variable i : integer;
    begin
        y := x + 1;
    end procedure;

    procedure bar(file x : text);

    procedure baz is
        variable y : integer;
        type foo;
        alias x is y;
        constant k : integer := 2;
    begin
    end procedure;

    procedure tralala is
        use work.p;
    begin
    end procedure;

end package body;
