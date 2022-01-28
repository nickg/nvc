package TYPES is

    type      SHAPE_TYPE    is record
                  ELEM_BITS :  integer;
                  X         :  integer;
                  Y         :  integer;
    end record;
    function  NEW_SHAPE(ELEM_BITS,X,Y: integer) return SHAPE_TYPE;

    type      STRIDE_TYPE   is record
                  X         :  integer;
                  Y         :  integer;
    end record;
    function  NEW_STRIDE(X,Y:integer) return STRIDE_TYPE;

    type      PARAM_TYPE    is record
                  ELEM_BITS :  integer;
                  INFO_BITS :  integer;
                  STRIDE    :  STRIDE_TYPE;
                  SHAPE     :  SHAPE_TYPE;
    end record;
    function  NEW_PARAM(
                  ELEM_BITS :  integer;
                  INFO_BITS :  integer := 0;
                  SHAPE     :  SHAPE_TYPE;
                  STRIDE    :  STRIDE_TYPE)
                  return       PARAM_TYPE;
    function  NEW_PARAM(
                  ELEM_BITS :  integer;
                  INFO_BITS :  integer := 0;
                  SHAPE     :  SHAPE_TYPE)
                  return       PARAM_TYPE;
end TYPES;

use     WORK.TYPES.all;
entity  TEST_NG is
end     TEST_NG;
architecture MODEL of TEST_NG is
    constant  ELEM_BITS    :  integer :=  8;
    constant  PARAM        :  PARAM_TYPE
                           := NEW_PARAM(
                                  ELEM_BITS => ELEM_BITS,
                                  SHAPE     => NEW_SHAPE(ELEM_BITS,3,3)
                              );
begin
end MODEL;
