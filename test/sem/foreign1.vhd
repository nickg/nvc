package p is
    procedure fproc;
    attribute foreign of fproc : procedure is "_foo";

    procedure proc;
end package;

package body p is

    procedure proc is
    begin
        -- This procedure should be tagged as never waiting
        fproc;
    end procedure;

end package body;
