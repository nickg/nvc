entity e is end entity;

architecture a of e is
    signal x : integer;
begin

    bad syntax;

    x <= 2; x <= 1 + 2;                 -- Recovery

    some more bad syntax;

    x <= 2; x <= 1 + 2;                 -- Recovery

    process
    begin
    end;                                -- Missing "process"

    x <= 2; x <= 1 + 2;                 -- Recovery

    foo: process is
    begin
    end process bar;                    -- Label does not match

    process is
    begin
    end process bar;                    -- No initial label

    b: block is
        impure function "+" return boolean is
        begin
            my_if: if x > 5 then
                null;
            end if blah;                -- Label does not match
            x <= 2; x <= 1 + 2;         -- Recovery
        end function "-";               -- Label does not match
    begin
    end block;

    p1: process is begin end process;
    p1: process is begin end process;   -- Error, duplicate label

    a1: assert x = 1;
    a1: assert x = 2;                   -- Error, duplicate label

    s1: x <= 5;
    s1: x <= 6;                         -- Error, duplicate label

    b1: block is begin end block;
    b1: block is begin end block;       -- Error, duplicate label

    b2: block is
        procedure proc(x : integer) is begin end procedure;
    begin
        c1: proc(1);
        c1: proc(2);                    -- Error, duplicate label
    end block;

    c1: not_a_library                   -- Error
        port map ( x => 1 );

end architecture;

architecture bad of not_here is         -- Error
begin
end architecture;
