entity protected3 is
end entity;

architecture test of protected3 is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
    end protected SharedCounter;

    type SharedCounter is protected body
        variable counter: Integer := 0;
        variable dummy: Integer;

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
    end protected body;

    procedure do_inc(p : inout SharedCounter) is
    begin
        p.increment;
    end procedure;

    shared variable x : SharedCounter;

begin

    process is
    begin
        do_inc(x);
        assert x.value = 1;
        wait;
    end process;

end architecture;
