package pack is
    function  MAX(A,B:integer) return integer;

    function  CALC_DATA_SIZE(WIDTH:integer) return integer;
end package;

package body pack is

    function  MAX(A,B:integer) return integer is begin
        if (A > B) then return A;
        else            return B;
        end if;
    end function;

    function  CALC_DATA_SIZE(WIDTH:integer) return integer is
        variable value : integer;
    begin
        value := 0;
        -- Crashed here as result of ** was real
        while (2**(value) < WIDTH) loop
            value := value + 1;
        end loop;
        return value;
    end function;
end package body;

use work.pack.all;

entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is

    constant  T_DATA_WIDTH      : integer := 12;
    constant  M_DATA_WIDTH      : integer := 23;
    constant  BUF_DATA_BITS     : integer := MAX(T_DATA_WIDTH, M_DATA_WIDTH);
    constant  BUF_DATA_BIT_SIZE : integer := CALC_DATA_SIZE(BUF_DATA_BITS);

begin
end;
