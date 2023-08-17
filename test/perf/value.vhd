package value is
    impure function test_string2int return integer;
    impure function test_string2real return real;
    impure function test_string2phys return time;
end package;

package body value is

    impure function test_string2int return integer is
        variable s : string(1 to 10);
    begin
        s := "1234567890";
        return integer'value(s);
    end function;

    impure function test_string2real return real is
        variable s : string(1 to 11);
    begin
        s := "15.12511e-7";
        return real'value(s);
    end function;

    impure function test_string2phys return time is
        variable s : string(1 to 8);
    begin
        s := "16231 ns";
        return time'value(s);
    end function;

end package body;
