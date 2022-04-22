entity issue101 is
end entity;

architecture SIGN of issue101 is
    signal TRIGGER, RESULT: integer := 0;
    signal signal1: integer :=1;
    signal signal2: integer :=2;
    signal signal3: integer :=3;
begin
    process (signal1, signal2, signal3)
    begin
        -- wait on TRIGGER;
        signal1 <= signal2;
        signal2 <= signal1 + signal3;
        signal3 <= signal2;
        RESULT <= signal1 + signal2 + signal3;
    end process;
monitor:
    process(RESULT)
    begin
        report "RESULT = " & integer'image(RESULT);
    end process;
end SIGN;
