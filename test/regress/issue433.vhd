entity  SAMPLE is
    generic (NAME: string := "");
end entity;
architecture MODEL of SAMPLE is
begin
    process begin
        assert (FALSE) report NAME & ":OK" severity NOTE;
        wait;
    end process;
end MODEL;

entity  SAMPLE_NG is
    generic (NAME: string := "");
end entity;
architecture MODEL of SAMPLE_NG is
    component SAMPLE generic(NAME: STRING := ""); end component;
begin
    U: SAMPLE generic map (NAME => NAME & "(" & ")");
end MODEL;

entity  issue433 is
    generic (NAME: string := "TEST_NG");
end entity;
architecture MODEL of issue433 is
    component SAMPLE_NG generic(NAME: STRING := ""); end component;
begin
    U1: SAMPLE_NG generic map(NAME => NAME & string'(":U1"));
    U2: SAMPLE_NG generic map(NAME => NAME & string'(":U2"));
end MODEL;
