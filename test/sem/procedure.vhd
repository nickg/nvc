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

    procedure foo_wrap(y : out integer) is
    begin
        foo(5, y);
    end procedure;

    procedure has_def(x : in integer; y : in integer := 7) is
    begin
    end procedure;

    procedure calls_has_def is
    begin
        has_def(5);
    end procedure;

    procedure bad_def(x : in bit := 6) is
    begin
    end procedure;

    procedure bad_def2(x : in bit := '1'; y : in integer) is
    begin
    end procedure;

end package body;
