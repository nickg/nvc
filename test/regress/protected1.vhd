entity protected1 is
end entity;

architecture test of protected1 is

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

    shared variable x : SharedCounter;

begin

    process is
    begin
        assert x.value = 0;
        x.increment;
        report "value is now " & integer'image(x.value);
        x.increment(2);
        assert x.value = 3;
        wait;
    end process;

    process is
    begin
        wait for 1 ns;
        assert x.value = 3;
        x.decrement;
        assert x.value = 2;
        wait;
    end process;

end architecture;
