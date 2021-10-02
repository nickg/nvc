entity resfn1 is
end entity;

architecture test of resfn1 is

    function resolved(v : bit_vector) return bit is
    begin
        return '1';
    end function;

    subtype rbit is resolved bit;

    signal x, y : rbit;
begin

end architecture;
