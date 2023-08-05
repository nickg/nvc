package synopsys1 is
    type std_ulogic_vector is array (natural range <>) of bit;
    type line is access string;
    type side is (left, right);
    subtype width is natural;

    procedure WRITE(L:inout LINE; VALUE:in STD_ULOGIC_VECTOR;
                    JUSTIFIED:in SIDE := RIGHT; FIELD:in WIDTH := 0);
end package;

package body synopsys1 is

    type char_indexed_by_MVL9 is array (bit) of character;
    constant MVL9_to_char: char_indexed_by_MVL9 := "01";

    procedure WRITE(L:inout LINE; VALUE:in STD_ULOGIC_VECTOR;
                    JUSTIFIED:in SIDE := RIGHT; FIELD:in WIDTH := 0) is
        variable s: string(1 to value'length);
        variable m: STD_ULOGIC_VECTOR(1 to value'length) := value;
    begin
        for i in 1 to value'length loop
            s(i) := MVL9_to_char(m(i));
        end loop;
        --write(l, s, justified, field);
    end WRITE;

end package body;
