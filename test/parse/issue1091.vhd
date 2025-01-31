entity adderN is
    port(addend: in boolean);
end;

architecture struct_adderN of adderN is
    component adder is
        port(addend: in boolean);
    end component;
begin
    u: adder port map(addend'last_value => addend);
end;

--------------------------------------------------------------------------------

PACKAGE Vital_Memory IS END PACKAGE Vital_Memory;
PACKAGE BODY Vital_Memory IS
    PROCEDURE InternalTimingCheck (CONSTANT SetupHigh : IN TIME := 0 ns) IS
    BEGIN
    END InternalTimingCheck;

    PROCEDURE VitalMemorySetupHoldCheck (SIGNAL TestSignal : IN time) IS
    BEGIN
        InternalTimingCheck (WORK.SetupHigh => TestSignal);
    END VitalMemorySetupHoldCheck;
END PACKAGE BODY Vital_Memory;
