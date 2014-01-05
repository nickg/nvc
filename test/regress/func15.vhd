entity func15 is
end entity;

architecture test of func15 is

    -- Generated invalid LLVM IR
    function outer(d : bit_vector(7 downto 0)) return bit is
        function inner(x : in bit) return bit is
        begin
            return not x;
        end function;
    begin
        return inner(d(2));
    end function;

begin

    process is
    begin
        assert outer(X"ff") = '0';
        wait;
    end process;

end architecture;
