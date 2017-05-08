use     std.textio.all;
entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is
    type      TEST_TYPE is record
        name : LINE;
    end record;
    function  NEW_TEST(NAME: STRING) return TEST_TYPE is
        variable self : TEST_TYPE;
    begin
        WRITE(self.name, NAME);
        return self;
    end function;
begin
    process
        -- Crashed here as cannot evaluate vcode null
        -- ... but also an access type cannot be constant folded
        variable t : TEST_TYPE := NEW_TEST(string'("test"));
    begin
    end process;
end MODEL;
