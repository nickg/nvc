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

    type bad2 is protected body         -- Error
    end protected body;

    type integer is protected body      -- Error
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

    shared variable y : SharedCounter := 1;  -- Error

begin

end architecture;
