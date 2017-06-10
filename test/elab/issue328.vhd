-- test_ng.vhd
entity  TEST_NG is
    generic (
        INFO_BITS  : integer :=  1
    );
    port (
        I_INFO_0   : in  bit_vector(INFO_BITS-1 downto 0);
        I_INFO_1   : in  bit_vector(INFO_BITS-1 downto 0);
        O_INFO_0   : out bit_vector(INFO_BITS-1 downto 0);
        O_INFO_1   : out bit_vector(INFO_BITS-1 downto 0)
    );
end     TEST_NG;
architecture MODEL of TEST_NG is
    type      INFO_RANGE_TYPE is record
              DATA_LO           : integer;
              DATA_HI           : integer;
    end record;
    type      VEC_RANGE_TYPE is record
              DATA_LO           : integer;
              DATA_HI           : integer;
              INFO_0            : INFO_RANGE_TYPE;
              INFO_1            : INFO_RANGE_TYPE;
    end record;
    function  SET_VEC_RANGE return VEC_RANGE_TYPE is
        variable  d_pos         : integer;
        variable  v             : VEC_RANGE_TYPE;
        procedure SET_INFO_RANGE(INFO_RANGE: inout INFO_RANGE_TYPE; BITS: in integer) is
        begin
            INFO_RANGE.DATA_LO  := d_pos;
            INFO_RANGE.DATA_HI  := d_pos + BITS-1;
            d_pos := d_pos + BITS;
        end procedure;
    begin
        d_pos := 0;
        v.DATA_LO := d_pos;
        SET_INFO_RANGE(v.INFO_0, INFO_BITS);
        SET_INFO_RANGE(v.INFO_1, INFO_BITS);
        v.DATA_HI := d_pos - 1;
        return v;
    end function;
    constant  VEC_RANGE      : VEC_RANGE_TYPE  := SET_VEC_RANGE;
    signal    i_data         : bit_vector(VEC_RANGE.DATA_HI downto VEC_RANGE.DATA_LO);
begin
    i_data(VEC_RANGE.INFO_0.DATA_HI downto VEC_RANGE.INFO_0.DATA_LO) <= I_INFO_0;
    i_data(VEC_RANGE.INFO_1.DATA_HI downto VEC_RANGE.INFO_1.DATA_LO) <= I_INFO_1;
    O_INFO_0 <= i_data(VEC_RANGE.INFO_0.DATA_HI downto VEC_RANGE.INFO_0.DATA_LO);
    O_INFO_1 <= i_data(VEC_RANGE.INFO_1.DATA_HI downto VEC_RANGE.INFO_1.DATA_LO);
end MODEL;
