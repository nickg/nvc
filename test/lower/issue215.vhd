entity  SUB is
    port (I:in integer;O:out integer);
end     SUB;
architecture MODEL of SUB is
begin
    process(I)
        procedure PROC_A(I:in integer;O:out integer) is
            procedure PROC_B(I:in integer;O:out integer) is
            begin
                O := I+1;
            end procedure;
        begin
            PROC_B(I,O);
        end procedure;
        variable oo : integer;
    begin
        PROC_A(I,oo);
        O <= oo;
    end process;
end MODEL;
entity  TOP is
end     TOP;
architecture MODEL of TOP is
    component SUB is
        port (I:in integer;O:out integer);
    end component;
    signal A_I, A_O : integer;
    signal B_I, B_O : integer;
begin
    A: SUB port map(I => A_I, O => A_O);
    B: SUB port map(I => B_I, O => B_O);
end MODEL;
