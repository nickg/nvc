entity  SAMPLE is
    generic (
        DATA_BITS : integer := 0
    );
    port (
        DATA : bit_vector(DATA_BITS-1 downto 0)
    );
end entity;
architecture RTL of SAMPLE is
begin
end RTL;

entity  issue437 is
    generic(
        DATA_BITS   : integer :=  8
   );
end entity;
architecture MODEL of issue437 is
    type      VEC_RANGE_TYPE is record
              VAL_LO            : integer;
              VAL_HI            : integer;
              DATA_LO           : integer;
              DATA_HI           : integer;
    end record;
    function  SET_VEC_RANGE return VEC_RANGE_TYPE is
        variable  v_pos         : integer;
        variable  d_pos         : integer;
        variable  v             : VEC_RANGE_TYPE;
        procedure SET_DATA_RANGE(DATA_RANGE: inout VEC_RANGE_TYPE; BITS: in integer) is
        begin
            v_pos := v_pos + 1;
            d_pos := d_pos + BITS + 1;
        end procedure;
    begin
        v_pos := 0;
        d_pos := 0;
        v.VAL_LO  := v_pos;
        v.DATA_LO := d_pos;

        SET_DATA_RANGE(v, DATA_BITS);

        v.VAL_HI  := v_pos - 1;
        v.DATA_HI := d_pos - 1;

        return v;
    end function;
    constant  VEC_RANGE      :  VEC_RANGE_TYPE  := SET_VEC_RANGE;
    signal    data           :  bit_vector(VEC_RANGE.DATA_HI downto VEC_RANGE.DATA_LO);
begin
    U: entity WORK.SAMPLE generic map(DATA_BITS => data'length) port map(DATA => data);
end MODEL;
