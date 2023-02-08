entity issue612 is
end entity;

architecture test of issue612 is

    procedure many_args (arg0, arg1, arg2, arg3, arg4 : out integer;
                         arg5, arg6, arg7, arg8, arg9 : out integer;
                         arg10, arg11, arg12, arg13, arg14 : out integer;
                         arg15, arg16, arg17, arg18, arg19 : out integer;
                         arg20, arg21, arg22, arg23 : out integer);

    attribute foreign of many_args : procedure is "VHPIDIRECT many_args";

begin

    p1: process is
        variable v0, v1, v2, v3, v4 : integer;
        variable v5, v6, v7, v8 : integer;
        variable v9, v10, v11, v12 : integer;
        variable v13, v14, v15, v16 : integer;
        variable v17, v18, v19, v20 : integer;
        variable v21, v22, v23 : integer;
    begin
        many_args(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10,
                  v11, v12, v13, v14, v15, v16, v17, v18, v19,
                  v20, v21, v22, v23);
        wait;
    end process;

end architecture;
