package pack is
    type int_ptr is access integer;
    type rec is record
        x : int_ptr;
    end record;
    type ft is file of character;

    type pt is protected
        procedure proc1 (x : inout int_ptr);  -- OK
        procedure proc2 (variable x : in rec);   -- OK
        function func1 (file x : ft) return character;  -- OK
        function func2 return int_ptr;  -- OK
    end protected;
end package;
