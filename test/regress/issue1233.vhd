ENTITY issue1233 IS
BEGIN
END ENTITY;

ARCHITECTURE arch OF issue1233 IS
    PROCEDURE intout (i : OUT INTEGER) IS
    BEGIN
        REPORT "ERROR: foreign subprogram 'vhpi_intout' not called" SEVERITY failure;
    END PROCEDURE;
    ATTRIBUTE foreign OF intout : PROCEDURE IS "VHPI vhpi.so vhpi_intout";
    --
    PROCEDURE rangedintout (i : OUT INTEGER RANGE 0 TO 5) IS
    BEGIN
        REPORT "ERROR: foreign subprogram 'vhpi_rangedintout' not called" SEVERITY failure;
    END PROCEDURE;
    ATTRIBUTE foreign OF rangedintout : PROCEDURE IS "VHPI vhpi.so vhpi_rangedintout";
BEGIN
    test : PROCESS IS
        VARIABLE i : INTEGER := 0;
        VARIABLE j : INTEGER := 0;
    BEGIN
        intout(i);
        ASSERT i = 1;
        rangedintout(j);
        ASSERT j = 1; -- fails
        WAIT;
    END PROCESS;
END ARCHITECTURE;
