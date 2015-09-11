architecture a of e is
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
        function "+" return boolean is
        begin
            my_if: if x > 5 then
                null;
            end if blah;                -- Label does not match
            x <= 2; x <= 1 + 2;         -- Recovery
        end function "-";               -- Label does not match
    begin
    end block;

end architecture;
