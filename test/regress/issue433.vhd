entity  SAMPLE is
    generic (NAME: string := ""; delay : delay_length);
end entity;
architecture MODEL of SAMPLE is
begin
    process begin
        wait for delay;
        assert (FALSE) report NAME & ":OK" severity NOTE;
        wait;
    end process;
end MODEL;

entity  SAMPLE_NG is
    generic (NAME: string := ""; delay : delay_length);
end entity;
architecture MODEL of SAMPLE_NG is
    component SAMPLE generic(NAME: STRING := ""; delay : delay_length); end component;
begin
    U: SAMPLE generic map (NAME => NAME & "(" & ")", delay => delay);
end MODEL;

entity  issue433 is
    generic (NAME: string := "TEST_NG");
end entity;
architecture MODEL of issue433 is
    component SAMPLE_NG generic(NAME: STRING := ""; delay : delay_length); end component;
begin
    U1: SAMPLE_NG generic map(NAME => NAME & string'(":U1"), delay => 1 ns);
    U2: SAMPLE_NG generic map(NAME => NAME & string'(":U2"), delay => 2 ns);
end MODEL;
