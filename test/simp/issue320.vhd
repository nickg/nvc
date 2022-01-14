package TEST_PACKAGE is
    type      CHANNEL_TYPE is (
        CHANNEL_0,
        CHANNEL_1,
        CHANNEL_2
    );
end     TEST_PACKAGE;

use     WORK.TEST_PACKAGE.all;
entity  ISSUE320 is
end     ISSUE320;
architecture MODEL of ISSUE320 is
    function  GEN_INIT_VALUE(CHANNEL: CHANNEL_TYPE) return integer is
        variable  value : integer;
    begin
        case CHANNEL is
            when CHANNEL_1 => value := 1;
            when CHANNEL_2 => value := 2;
            when others    => value := 0;
        end case;
        return value;
    end function;
    -- Strange error about not being able to find a libary as '.' in
    -- mangled function name wrongly interpreted
    constant  INIT_VALUE : integer := GEN_INIT_VALUE(CHANNEL_0);
begin
end MODEL;
