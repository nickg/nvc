package p is

    type SharedCounter is protected
        procedure increment (N: Integer := 1);
        procedure decrement (N: Integer := 1);
        impure function value return Integer;
    end protected SharedCounter;

    type ComplexNumber is protected
        procedure extract (variable r, i: out Real);
        procedure add (variable a, b: inout ComplexNumber);
    end protected ComplexNumber;

    type VariableSizeBitArray is protected
        procedure add_bit (index: Positive; value: Bit);
        impure function size return Natural;
    end protected VariableSizeBitArray;

end package;
