entity sig2var is
end entity;

architecture test of sig2var is

    function foo(signal x : bit_vector) return bit_vector is
        variable v : bit_vector(1 to 8) := x;
    begin
        return v;
    end function;

    function bar(signal x : bit) return bit is
        variable v : bit := x;
    begin
        return v;
    end function;

begin

end architecture;
