package pack is
    type int_ptr is access integer;
    impure function func1 (variable x : inout int_ptr) return integer;  -- OK
    pure function func2 (x : inout int_ptr) return integer;  -- Error
    impure function func3 (variable x : in integer) return integer;  -- OK
    pure function func4 (variable x : in integer) return integer;  -- Error
end package;
