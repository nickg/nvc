package trace1 is
    procedure test1;
end package;

package body trace1 is

    procedure test3 is
        variable x : integer;
    begin
        x := 1 / 0;                     -- Error
    end procedure;

    procedure test1 is
        procedure test2 is
        begin
            test3;
        end procedure;
    begin
        test2;
    end procedure;

end package body;
