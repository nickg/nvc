entity osvvm2 is
end entity;

architecture test of osvvm2 is

    type line is access string;

    type FieldNameArrayType is array (natural range <>) of Line ;

    procedure test (Dimensions : in natural) is
        variable FieldNameArray : FieldNameArrayType(1 to 20);

        type PtrType is access FieldNameArrayType;
        variable FieldName : PtrType;
    begin
        FieldName := new FieldNameArrayType'(FieldNameArray(1 to Dimensions));
    end procedure;

begin
end architecture;
