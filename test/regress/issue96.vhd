entity issue96 is
end entity;

architecture foo of issue96 is
    function "&" (l: string; r: natural) return string is
    begin
        report "called the user function";
        assert r <= 255
            report  "&, natural " & natural'IMAGE(r) &
                  " argument out of character range"
            severity ERROR;
        return  l & character'VAL(r);
    end function;

    constant a: string := "abcd";
    constant b: string := "efgh";
begin
    assert FALSE
    report "concatenated string is " & a &  -- Line 18
            character'VAL(16#42#) & b &
            "  " &  a & 16#42# & b
    severity NOTE;

end architecture;
