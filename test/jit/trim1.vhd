package trim1 is
    function func1 return bit_vector;
    function func2 return bit;
    function func3(n : integer) return character;
    impure function func4 return bit_vector;
    procedure proc1;
    function func5(n : integer) return integer;
    impure function func6 return bit;
    function func7(n : integer) return integer;
end package;

package body trim1 is

    -- Returns local variable
    function func1 return bit_vector is
        variable v : bit_vector(1 to 3);
    begin
        return v;
    end function;

    -- Returns scalar but calls func1 which has local allocation
    function func2 return bit is
    begin
        return func1(1);
    end function;

    -- Local allocation but returns scalar
    function func3(n : integer) return character is
        variable v : bit_vector(1 to n);
    begin
        return to_string(v)(n);         -- Prevent optimisation
    end function;

    -- Inner function returns outer local variable
    impure function func4 return bit_vector is
        variable v : bit_vector(1 to 3);

        impure function inner return bit_vector is
        begin
            return v(1 to 2);
        end function;
    begin
        return inner;
    end function;

    -- Nested function call has local allocation
    procedure proc1 is
    begin
        assert func1 = "000";
    end procedure;

    function sum(a : integer_vector) return integer is
        variable cnt : integer := 0;
    begin
        for i in a'range loop
            cnt := cnt + a(i);
        end loop;
        return cnt;
    end function;

    function iota(n : integer) return integer_vector is
        variable r : integer_vector(1 to n);
    begin
        for i in r'range loop
            r(i) := i;
        end loop;
        return r;
    end function;

    -- Multiple calls with local allocations
    function func5(n : integer) return integer is
        constant c : integer_vector := iota(n);
    begin
        return sum(c);
    end function;

    -- Inner function uses outer variable but returns scalar
    impure function func6 return bit is
        variable v : bit_vector(1 to 3);

        impure function inner return bit_vector is
        begin
            return v(1 to 2);
        end function;
    begin
        return not inner(1);
    end function;

    -- Local non-variable allocation but returns scalar
    function func7(n : integer) return integer is
    begin
        return sum((1 to n => 1));
    end function;

end package body;
