package pack is
    procedure do_something;
    signal s : bit;
end package;

package body pack is
    procedure drive_signal_out (signal p : out bit) is
    begin
        p <= '1';
    end procedure;

    procedure drive_signal_inout (signal p : inout bit) is
    begin
        p <= '1';
    end procedure;

    procedure do_something is
    begin
        s <= '0';                       -- This errors...
        drive_signal_out(s);            -- ... but not this
        drive_signal_inout(s);          -- ... or this
    end procedure;
end package body;
