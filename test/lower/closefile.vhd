package filepack is
    procedure test;
end package;

package body filepack is

    procedure test is
        type text is file of string;
        file F: TEXT open write_mode is "f";
    begin
        -- F should be closed before returning
    end procedure;

end package body;
