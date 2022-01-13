entity  issue436 is
end entity;
architecture MODEL of issue436 is
    type      VEC_RANGE_TYPE is record
              DATA_LO        :  integer;
              DATA_HI        :  integer;
    end record;
    function  SET_VEC_RANGE return VEC_RANGE_TYPE is
        variable  v          :  VEC_RANGE_TYPE;
    begin
        v.DATA_HI := 31;
        v.DATA_LO :=  0;
        return v;
    end function;
    constant  VEC_RANGE      :  VEC_RANGE_TYPE  := SET_VEC_RANGE;
    signal    data           :  bit_vector(VEC_RANGE.DATA_HI downto VEC_RANGE.DATA_LO);

    type int_vector is array (natural range <>) of integer;

    function get_array return int_vector is
        variable v : int_vector(1 to 3);
    begin
        v := (1, 2, 3);
        return v;
    end function;

    constant c2 : int_vector := get_array;
    signal data2 : bit_vector(c2(2) downto 0);
begin
end MODEL;
