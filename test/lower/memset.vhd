entity memset is
end entity;

architecture test of memset is

    type int_vector is array (integer range <>) of integer;

    function foo(x : positive) return bit_vector is
        variable v : bit_vector(1 to x);
    begin
        return v;
    end function;

    function bar(x : positive) return int_vector is
        variable v : int_vector(1 to x) := (others => 16#abababab#);
    begin
        return v;
    end function;

begin

end architecture;
