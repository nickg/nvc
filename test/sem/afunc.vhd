entity afunc is
end entity;

architecture test of afunc is

    function get return bit_vector is
    begin
        return X"10";
    end function;

    function get2(x : integer := 0) return bit_vector is
    begin
        return "01";
    end function;

    function get3(x, y : integer := 0) return bit_vector is
    begin
        return "01";
    end function;
begin

    process is
    begin
        assert get(0) = '0';            -- OK
        assert '0' = get(0);            -- OK
        assert get2(0) = "01";          -- OK
        assert "01" = get2(0);          -- OK
        assert get2(0) = '0';           -- OK
        assert '0' = get2(0);           -- OK
        assert get3(1, 2) & '1' = "01"; -- OK
        wait;
    end process;

    -- Reduced from VESTS case tc1072.vhd
    b1: block is
        TYPE    A  IS ARRAY (NATURAL RANGE <>) OF INTEGER;
        SUBTYPE    A6 IS A (1 TO 6);
        SUBTYPE    A8 IS A (1 TO 8);

        FUNCTION func1 (a,b : INTEGER := 3) RETURN A6 IS
        BEGIN
            IF (a=3) AND (b=3) THEN
                RETURN (1,2,3,4,5,6);
            ELSE
                IF (a=3) THEN
                    RETURN (11,22,33,44,55,66);
                ELSE
                    RETURN (111,222,333,444,555,666);
                END IF;
            END IF;
        END;

    begin
        TESTING: PROCESS
            VARIABLE q : A8;
        BEGIN
            q(1) := func1(3)(1);        -- OK
        end process;
    end block;

    -- Seen in OSVVM
    process is
        function get (prefix : string := ""; n : integer := 0) return string is
        begin
            return prefix;
        end function;

        constant s1 : string := "foo" & get("hello") & '1';  -- OK
--        constant s2 : string := "foo" & get(1) & '1';  -- OK
        constant s3 : string := get(n => 5) & "1";  -- OK

        type pt is protected
            function get1 (prefix : string := "") return string;
            procedure test;
        end protected;

        type pt is protected body
            function get1 (prefix : string := "") return string is
            begin
                return prefix;
            end function;

            procedure test is
                constant c1 : string := "foo" & get1("hello") & '1';  -- OK
                constant c2 : string := "foo" & get1(1) & '1';  -- OK
            begin
            end procedure;
        end protected body;
    begin
    end process;

end architecture;
