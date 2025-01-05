package pack1 is
    procedure foo is new bar.all;       -- Error
end package;

package pack2 is end package;

package body pack2 is
    procedure adder generic (type t) is
        type fnat is file of natural;
    begin
    end procedure;
    procedure addi is new adder generic map (t => integer);  -- OK
end package body;
