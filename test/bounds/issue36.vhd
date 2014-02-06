entity bounds18 is
    generic (
        W : integer range 1 to integer'high := 8
    );
    function func2(x : integer; w : natural) return integer is
    begin
        return x + w;
    end func2;

    pure function fA (
        iA : integer range 0 to 2**W-1
    ) return integer is
    begin
        return func2(iA, W);
    end function fA;
begin
    assert (fA(0) = 0) report "should not assert" severity failure;
end entity bounds18;
