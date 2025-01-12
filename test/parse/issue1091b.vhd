PACKAGE Vital_Memory IS END Vital_Memory;

PACKAGE BODY Vital_Memory IS
    PROCEDURE MemoryTableCorruptMask (
        VARIABLE CorruptMask : OUT INTEGER
    ) IS
    BEGIN
    END;

    PROCEDURE MemoryTableLookUp (
        VARIABLE DataCorruptMask : OUT INTEGER;
        CONSTANT EnableIndex     : IN INTEGER
    ) IS
    BEGIN
        MemoryTableCorruptMask (
            CorruptMask => DataCorruptMask,
            EnableIndex => EnableIndex
        );
    END;

END PACKAGE BODY Vital_Memory;
