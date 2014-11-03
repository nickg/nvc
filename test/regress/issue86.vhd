entity issue86 is
end entity;

architecture test of issue86 is
    type integer_vector is array (natural range <>) of integer;
    subtype ElementType        is integer ;
    subtype ArrayofElementType is integer_vector;

    function inside0 (constant E : ElementType;
                      constant A : in ArrayofElementType) return boolean is
    begin
        for i in A'range loop
            if E = A(i) then
                return TRUE;
            end if ;
        end loop ;
        return FALSE ;
    end function inside0;
begin

    process is
        variable a : ArrayofElementType(1 to 5);
    begin
        a(3) := 2;
        assert not inside0(3, a);
        assert inside0(2, a);
        wait;
    end process;

end architecture;
