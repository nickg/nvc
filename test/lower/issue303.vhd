entity  TEST_NG is
    port (
        DATA_IN  : in  bit;
        DATA_OUT : out bit
    );
end TEST_NG;
architecture MODEL of TEST_NG is
    procedure  wait_on_signals is
    begin
        wait on DATA_IN;                -- Crash when lowering architecture
    end procedure;
begin
end MODEL;
