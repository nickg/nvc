package CORE is
    type      STATUS_TYPE is record
                  error_count  : integer;
    end record;
end CORE;

library WORK;
use     WORK.CORE.STATUS_TYPE;
entity  TEST_1 is
    generic (
        EXP_STATUS      : STATUS_TYPE
    );
end     TEST_1;
architecture MODEL of TEST_1 is
begin
end MODEL;

library WORK;
use     WORK.CORE.STATUS_TYPE;
entity  TEST_1_1 is
end     TEST_1_1;
architecture MODEL of TEST_1_1 is
    constant EXP_STATUS : STATUS_TYPE := (error_count =>  0);
    component TEST_1
        generic (
            EXP_STATUS  : STATUS_TYPE
        );
    end component;
begin
    U: TEST_1 generic map(EXP_STATUS);
end MODEL;
