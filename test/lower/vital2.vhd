package vital_timing is
    -- Types for fields of VitalTimingDataType
    TYPE VitalTimeArrayT  IS ARRAY (INTEGER RANGE <>) OF TIME;
    TYPE VitalTimeArrayPT IS ACCESS VitalTimeArrayT;
    TYPE VitalBoolArrayT  IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
    TYPE VitalBoolArrayPT IS ACCESS VitalBoolArrayT;
    TYPE VitalLogicArrayPT IS ACCESS bit_vector;

    TYPE VitalTimingDataType IS RECORD
        NotFirstFlag : BOOLEAN;
        RefTime   : TIME;
        HoldEn    : BOOLEAN;
        TestLast  : bit;
        TestTime  : TIME;
        SetupEn   : BOOLEAN;
        TestLastA : VitalLogicArrayPT;
        TestTimeA : VitalTimeArrayPT;
        HoldEnA   : VitalBoolArrayPT;
        SetupEnA  : VitalBoolArrayPT;
    END RECORD;

    PROCEDURE VitalSetupHoldCheck (
            VARIABLE TimingData    : INOUT  VitalTimingDataType;
            SIGNAL   TestSignal    : IN     bit_vector;
    	    CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE  --IR252 3/23/98

    );
end package;

PACKAGE BODY VITAL_Timing IS
    PROCEDURE VitalSetupHoldCheck (
            VARIABLE TimingData    : INOUT  VitalTimingDataType;
            SIGNAL   TestSignal    : IN     bit_vector;
    	    CONSTANT EnableHoldOnRef   : IN   BOOLEAN := TRUE  --IR252 3/23/98

    ) IS
    BEGIN
        TimingData.HoldEnA.all := (TestSignal'RANGE => EnableHoldOnRef);	--IR252 3/23/98

    END VitalSetupHoldCheck;

END VITAL_Timing;
