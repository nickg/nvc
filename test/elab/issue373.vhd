entity  SUB is
    generic (NAME:STRING);
end SUB;
architecture MODEL of SUB is
begin
    process
    begin
        report NAME;
        wait;
    end process;
end MODEL;

entity  ISSUE373 is
end     ISSUE373;
architecture MODEL of ISSUE373 is
    component SUB generic (NAME:STRING); end component;
begin
    UNIT: SUB generic map (NAME => string'("ISSUE373:UNIT"));
end MODEL;
