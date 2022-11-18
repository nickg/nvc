package issue575 is
    type rec is record
        x : integer;
        y : bit_vector(1 to 3);
    end record;

    procedure test (x : out rec; val : bit);
end package;

package body issue575 is
    procedure test (x : out rec; val : bit) is
    begin
        x.y := (others => val);
    end procedure;
end package body;
