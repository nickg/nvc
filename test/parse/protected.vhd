package p is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
    end protected SharedCounter;

    type SharedCounter is protected body
        variable counter: Integer := 0;

        procedure increment (N: Integer := 1) is
        begin
            counter := counter + N;
        end procedure increment;

        procedure decrement (N: Integer := 1) is
        begin
            counter := counter - N;
        end procedure decrement;

        impure function value return Integer is
        begin
            return counter;
        end function value;

        procedure add10 is
        begin
            increment(10);
        end procedure;
    end protected body;

    procedure proc_1 (x : integer);

    type pt2 is protected
        procedure proc_1 (y : integer);
    end protected;

    type pt2 is protected body
        procedure proc_2 is
        begin
            proc_1(y => 5);              -- OK
        end procedure;
    end protected body;

end package;
