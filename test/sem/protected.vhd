entity e is
end entity;

architecture a of e is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
    end protected SharedCounter;

    type bad1 is protected
        procedure foo (x : not_here);   -- Error
    end protected;

    type bad1 is protected body         -- OK
    end protected body;

    type bad2 is protected body         -- Error
    end protected body;

    type boolean is protected body      -- Error
    end protected body;

    type now is protected body          -- Error
    end protected body;

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

    type SharedCounter is protected body  -- Error
    end protected body;

    subtype s is SharedCounter;         -- Error

    shared variable x : integer;        -- Error

    shared variable y : SharedCounter;  -- OK

    shared variable z : SharedCounter := 1;  -- Error

    function make return SharedCounter is  -- Error
        variable result : SharedCounter;
    begin
        return result;
    end function;

    procedure proc(variable sh : in SharedCounter := make) is   -- error
    begin
    end procedure;

begin

end architecture;

architecture a2 of e is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
        procedure foo (x : in integer);
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

        procedure bar (x : in integer ) is
        begin
            null;
        end procedure;

        procedure foo (x : in integer ) is
        begin
            bar(x + 1);
        end procedure;
    end protected body;

    shared variable x : SharedCounter;  -- OK

begin

    process is
    begin
        x.increment(2);                 -- OK
        x.increment;                    -- OK
        x.counter := 5;                 -- Error
        x.decrement(1, 2);              -- Error
        assert x.value = 5;             -- OK
    end process;

    process is
        function get_value (x : in sharedcounter ) return integer is  -- Error
        begin
            return x.value;             -- Error
        end function;
    begin
    end process;

    bad_assignment: process
        variable y : SharedCounter;
        variable z : SharedCounter;
    begin
        y := z;                             -- Error
        wait;
    end process;

end architecture;

package issue85 is

    type protected_t is protected
        procedure add(argument : inout protected_t);  -- OK
    end protected protected_t;

end package;

package pkg is
    type protected_t is protected
    end protected protected_t;
end package;

package body pkg is
    -- Missing body for protected_t
end package body;
