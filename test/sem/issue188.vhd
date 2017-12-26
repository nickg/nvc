entity issue188 is
end entity;

architecture test of issue188 is

    type ft is file of boolean;

    function file_func return boolean is
        file f : ft;                    -- Error
        variable b : boolean;
    begin
        read(f, b);
        return b;
    end function;

    impure function file_func_impure return boolean is
        file f : ft;                    -- OK
        variable b : boolean;
    begin
        read(f, b);
        return b;
    end function;

    file f : ft;

    function file_func2 return boolean is
        variable b : boolean;
    begin
        read(f, b);                     -- Error
        return b;
    end function;

    procedure read_b(b : out boolean) is
    begin
        read(f, b);
    end procedure;

    procedure call_read_b(b : out boolean) is
    begin
        read_b(b);
    end procedure;

    function call_call_read_b return boolean is
        variable b : boolean;
    begin
        call_read_b(b);                 -- Error
        return b;
    end function;

    impure function impure_call_call_read_b return boolean is
        variable b : boolean;
    begin
        call_read_b(b);                 -- Error
        return b;
    end function;

    shared variable x : integer;

    procedure update_x is
    begin
        x := 2;
    end procedure;

    function call_update_x return boolean is
    begin
        update_x;
        return true;
    end function;

    impure function impure_call_update_x return boolean is
    begin
        update_x;
        return true;
    end function;

begin

end architecture;
