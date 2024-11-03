entity issue1036 is
end entity;

architecture test of issue1036 is

    type int2d is array (natural range <>, natural range <>) of integer;

    procedure many_args
        ( a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11 : inout bit_vector;
          a12, a13, a14, a15, a16, a17, a18, a19 : inout integer;
          a20, a21, a22, a23, a24, a25, a26 : inout int2d;
          a27 : inout integer;
          signal a28 : inout bit_vector ) is
    begin
        assert a5 = X"05";
        assert a11 = X"1010";
        assert a25 = (0 => (0 => 42));
        assert a26(0, 0) = 99;
        assert a27 = 251;
        assert a28 = X"f0";

        a25(0, 0) := 77;
        a26(0, 0) := 55;
        a27 := 999;
        a28 <= not a28;
        wait for 0 ns;
    end procedure;

    signal s : bit_vector(7 downto 0) := X"f0";
begin

    process is
        variable v0, v1, v2, v3, v4, v5 : bit_vector(7 downto 0);
        variable v6, v7, v8, v9, v10, v11 : bit_vector(15 downto 0);
        variable v12, v13, v14, v15, v16, v17, v18, v19 : integer;
        variable v20, v21, v22, v23, v24, v25, v26 : int2d(0 to 0, 0 to 0);
        variable v27 : integer;
    begin
        v5 := X"05";
        v11 := X"1010";
        v25 := (0 => (0 => 42));
        v26(0, 0) := 99;
        v27 := 251;

        many_args(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11,
                  v12, v13, v14, v15, v16, v17, v18, v19,
                  v20, v21, v22, v23, v24, v25, v26, v27, s);

        assert v25 = (0 => (0 => 77));
        assert v26(0, 0) = 55;
        assert v27 = 999;
        assert s = X"0f";

        wait;
    end process;

end architecture;
