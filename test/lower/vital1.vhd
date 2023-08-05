package vital_timing is
    PROCEDURE VitalSetupHoldCheck (
            SIGNAL   TestSignal    : IN     bit_vector

    );
end package;

PACKAGE BODY VITAL_Timing IS

    type line is access string;

    procedure proc (variable x : line; i : integer) is
    begin
	wait for 1 ns;
    end procedure;

    ---------------------------------------------------------------------------
    PROCEDURE VitalSetupHoldCheck (
            SIGNAL   TestSignal    : IN     bit_vector

    ) IS
        VARIABLE StrPtr1 : LINE;

    BEGIN
        l1: FOR i IN TestSignal'RANGE LOOP

                proc (StrPtr1, i);
        END LOOP;
    END VitalSetupHoldCheck;


END VITAL_Timing;
