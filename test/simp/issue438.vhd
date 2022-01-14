-- test_ng.vhd
library ieee;
use     ieee.std_logic_1164.all;
entity  ISSUE438 is
    generic(
        INFO_SIZE  : integer := 1;
        INFO_BITS  : integer := 8
   );
end entity;
architecture MODEL of ISSUE438 is
    type      INFO_RANGE_TYPE is record
              VAL_POS           : integer;
              DATA_LO           : integer;
              DATA_HI           : integer;
    end record;
    type      INFO_RANGE_VECTOR is array (integer range <>) of INFO_RANGE_TYPE;
    type      VEC_RANGE_TYPE is record
              VAL_LO            : integer;
              VAL_HI            : integer;
              DATA_LO           : integer;
              DATA_HI           : integer;
              INFO_LIST         : INFO_RANGE_VECTOR(INFO_SIZE-1 downto 0);
    end record;
    function  SET_VEC_RANGE return VEC_RANGE_TYPE is
        variable  v_pos         : integer;
        variable  d_pos         : integer;
        variable  v             : VEC_RANGE_TYPE;
        procedure SET_INFO_RANGE(INFO_RANGE: inout INFO_RANGE_TYPE; BITS: in integer) is
        begin
            INFO_RANGE.VAL_POS  := v_pos;
            INFO_RANGE.DATA_LO  := d_pos;
            INFO_RANGE.DATA_HI  := d_pos + BITS-1;
            v_pos := v_pos + 1;
            d_pos := d_pos + BITS;
        end procedure;
    begin
        v_pos := 0;
        d_pos := 0;
        v.VAL_LO  := v_pos;
        v.DATA_LO := d_pos;

        for i in 0 to INFO_SIZE-1 loop
            SET_INFO_RANGE(v.INFO_LIST(i), INFO_BITS);
        end loop;

        v.VAL_HI  := v_pos - 1;
        v.DATA_HI := d_pos - 1;

        return v;
    end function;
    constant  VEC_RANGE      :  VEC_RANGE_TYPE  := SET_VEC_RANGE;
    signal    data           :  std_logic_vector(VEC_RANGE.DATA_HI downto VEC_RANGE.DATA_LO);
begin

end MODEL;
