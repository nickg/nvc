entity osvvm4 is
end entity;

architecture test of osvvm4 is

    procedure push (x : bit_vector) is
    begin
    end procedure;

    procedure push (y : string; x : bit_vector) is
    begin
    end procedure;
begin

    p1: process is
    begin
        push("101");                    -- OK
        push(x => "101");               -- OK
        push(('1', '0'));               -- OK
    end process;

end architecture;
