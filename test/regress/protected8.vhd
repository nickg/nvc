entity protected8 is
end entity;

architecture test of protected8 is

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
    end protected body;

    shared variable v : SharedCounter;

begin

    p1: v.increment(n => 5);

    p2: process is
    begin
        wait for 1 ns;
        assert v.value = 5;
        wait;
    end process;

end architecture;
